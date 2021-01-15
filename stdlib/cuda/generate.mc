include "cuda/ast.mc"
include "cuda/ast-builder.mc"
include "cuda/pprint.mc"
include "mexpr/ast.mc"

let _nameWithCudaTail = lam name.
  nameSym (join [nameGetStr name, "_cuda"])

let _nameWithKernelTail = lam name.
  nameSym (join [nameGetStr name, "_kernel"])

let _nameWithBytesTail = lam name.
  nameSym (join [nameGetStr name, "_bytes"])

let _isPtrType = lam ty.
  use CudaAst in
  match ty with CTyPtr { ty = _ } then true else false

-- Generates CAMLparam and CAMLxparam statements which are used to declare the
-- statements passsed from OCaml to the kernel function. When more than five
-- parameters are passed from OCaml, one or more calls to a separate function
-- have to be made.
let _generateCamlParamDecl = lam params.
  recursive let generateParamCalls = lam params. lam firstParams.
    let n = length params in
    let headParams = if lti n 5 then n else 5 in
    match splitAt params headParams with (lhs, rhs) then
      let argNames = map (lam p. cudavar_ p.1) lhs in
      let camlParamCall = (if firstParams then
        camlParams_ argNames
      else
        camlXparams_ argNames) in
      if null rhs then
        [camlParamCall]
      else
        cons camlParamCall (generateParamCalls rhs false)
    else never
  in
  generateParamCalls params true

lang CudaWrapperGenerate = CudaAst + MExprAst
  sem allocAndCopyToDevice =
  | (paramTy, paramName) ->
    let paramCudaName = _nameWithCudaTail paramName in
    match paramTy with CTyPtr { ty = ty } then
      let paramCudaDecl = CSDef {
        ty = paramTy,
        id = Some paramCudaName,
        init = None ()
      } in
      let paramNameBytes = _nameWithBytesTail paramName in
      let paramBytesDef = CSDef {
        ty = CTyInt (),
        id = Some paramNameBytes,
        init = Some ( CIExpr { expr = camlBaBytes_ paramName })
      } in
      let paramCudaMalloc = cudaMalloc_ [
        CEUnOp { op = COAddrOf (), arg = cudavar_ paramCudaName },
        cudavar_ paramNameBytes
      ] in
      let paramCudaMemcpy = cudaMemcpyH2D_ [
        cudavar_ paramCudaName,
        camlBaDataVal_ paramName,
        cudavar_ paramNameBytes
      ] in
      (paramCudaName, [
        paramCudaDecl, paramBytesDef, paramCudaMalloc, paramCudaMemcpy
      ])
    else match paramTy with CTyDouble () then
      let paramCudaDef = CSDef {
        ty = paramTy,
        id = Some paramCudaName,
        init = Some (CIExpr { expr = cudaApp_ _doubleVal [cudavar_ paramName] })
      } in
      (paramCudaName, [paramCudaDef])
    else error "Unsupported CUDA type"

  sem kernelCall (name: Name) (params: [Name]) =
  | out ->
    let n = nameSym "n" in
    let tpb = nameSym "tpb" in
    let blocks = nameSym "blocks" in
    let nDef = CSDef {
      ty = CTyInt (),
      id = Some n,
      init = Some (CIExpr { expr = camlBaElems_ out })
    } in
    let tpbDecl = CSDef {
      ty = CTyInt (),
      id = Some tpb,
      init = None ()
    } in
    let tpbMaxThreadsCall = cudaGetMaxThreadsPerBlock_ tpb in
    let blocksDef = CSDef {
      ty = CTyInt (),
      id = Some blocks,
      init = Some (CIExpr { expr = CEBinOp {
        op = CODiv (),
        lhs = CEBinOp {
          op = COSub (),
          lhs = CEBinOp {
            op = COAdd (),
            lhs = cudavar_ n,
            rhs = cudavar_ tpb
          },
          rhs = CEInt { i = 1 }
        },
        rhs = cudavar_ tpb
      }})
    } in
    let params = snoc params n in
    let kernelFunctionCall = CSExpr { expr = CudaEApp {
      fun = name,
      args = map (lam param. cudavar_ param) params,
      blocks = cudavar_ blocks,
      tpb = cudavar_ tpb
    }} in
    [nDef, tpbDecl, tpbMaxThreadsCall, blocksDef, kernelFunctionCall]

  sem cudaTemplate (name: Name) (params: [(CType, Name)]) =
  | outty ->
    let out = nameSym "out" in
    let outBytesName = _nameWithBytesTail out in
    let outCudaName = _nameWithCudaTail out in

    -- Declare all OCaml parameters
    let allParams = snoc params (outty, out) in
    let camlParam = _generateCamlParamDecl allParams in

    -- Allocate output array on device
    let outCudaDecl = CSDef {
      ty = outty,
      id = Some outCudaName,
      init = None ()
    } in
    let outBytes = CSDef {
      ty = CTyInt (),
      id = Some outBytesName,
      init = Some (CIExpr { expr = camlBaBytes_ out })
    } in
    let outCudaMalloc = cudaMalloc_ [
      CEUnOp { op = COAddrOf (), arg = cudavar_ outCudaName },
      cudavar_ outBytesName
    ] in

    -- For each argument (except output), move data host -> device
    let moveToDevice = map allocAndCopyToDevice params in
    let cudaParamNames = map (lam p. p.0) moveToDevice in
    let cudaCopyStmts = join (map (lam p. p.1) moveToDevice) in
    
    let kernelName = _nameWithKernelTail name in
    let kernelParams = snoc cudaParamNames outCudaName in
    let kernelStmts = kernelCall kernelName kernelParams out in

    -- Copy data to output array (device -> host)
    let deviceToHostMemcpy = cudaMemcpyD2H_ [
      camlBaDataVal_ out,
      cudavar_ outCudaName,
      cudavar_ outBytesName
    ] in

    -- Free all memory allocated on GPU
    let cudaAllocatedParamNames = snoc
      (map (lam p. p.1)
        (filter (lam p. _isPtrType p.0)
          (zipWith (lam p. lam cp. (p.0, cp)) params cudaParamNames)))
      outCudaName
    in
    let freeStmts = map cudaFree_ cudaAllocatedParamNames in

    -- Return to OCaml
    let returnStmt = camlReturn_ out in

    let stmts = join [
      camlParam, [outCudaDecl, outBytes, outCudaMalloc],
      cudaCopyStmts, kernelStmts, [deviceToHostMemcpy], freeStmts,
      [returnStmt]
    ] in
    CudaFun {
      ret = CTyIdent { id = _value },
      id = name,
      params = map (lam p. (valueTy_, p.1)) allParams,
      body = stmts,
      annot = [CudaExternC ()]
    }
end

mexpr

use CudaWrapperGenerate in
use CudaPrettyPrint in

let templateName = nameSym "scale" in
let params = [
  (CTyPtr { ty = CTyDouble ()}, nameSym "a"),
  (CTyDouble (), nameSym "s")
] in
let outty = CTyPtr { ty = CTyDouble () } in
let program = CPProg {
  includes = ["<caml/alloc.h>", "<caml/bigarray.h>", "<caml/memory.h>", "<caml/mlvalues.h>"],
  tops = [cudaTemplate templateName params outty]
} in

-- let _ = printLn (printCProg [] program) in

utest length (printCProg [] program) with 0 using geqi in

()

