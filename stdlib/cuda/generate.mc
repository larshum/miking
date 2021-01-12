include "cuda/ast.mc"
include "cuda/ast-builder.mc"
include "cuda/pprint.mc"
include "mexpr/ast.mc"

let nameWithCudaTail = lam name.
  nameSym (join [nameGetStr name, "_cuda"])

let nameWithKernelTail = lam name.
  nameSym (join [nameGetStr name, "_kernel"])

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
  -- TODO: implement copying for non-array types
  sem allocAndCopyToDevice =
  | (paramTy, paramName) ->
    let cudaParam = nameWithCudaTail paramName in
    (cudaParam, [
      CSDef {
        ty = CTyPtr { ty = valueTy_ },
        id = Some cudaParam,
        init = None ()
      },
      cudaMalloc_ [
        CEUnOp { op = COAddrOf (), arg = cudavar_ cudaParam },
        CEBinOp {
          op = COMul (),
          lhs = camlWosizeVal_ paramName,
          rhs = CESizeOfType { ty = valueTy_ }
        }
      ],
      cudaMemcpyH2D_ [
        cudavar_ cudaParam,
        camlOpVal_ paramName,
        CEBinOp {
          op = COMul (),
          lhs = camlWosizeVal_ paramName,
          rhs = CESizeOfType { ty = valueTy_ }
        }
      ]
    ])

  sem kernelCall (name: Name) (params: [Name]) =
  | n ->
    let tpb = nameSym "tpb" in
    let blocks = nameSym "blocks" in
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
    let kernelFunctionCall = CSExpr { expr = CudaEApp {
      fun = name,
      args = map (lam param. cudavar_ param) params,
      blocks = cudavar_ blocks,
      tpb = cudavar_ tpb
    }} in
    [tpbDecl, tpbMaxThreadsCall, blocksDef, kernelFunctionCall]

  sem cudaTemplate (name: Name) =
  | params ->
    let out = nameSym "out" in
    let cudaOut = nameSym "cuda_out" in

    let camlParam = _generateCamlParamDecl params in
    let camlLocal = cudaAppStmt_ (nameSym "CAMLlocal1") [cudavar_ out] in

    -- Define helper variable n which stores the size of the output array.
    let n = nameSym "n" in
    let nDecl = CSDef {
      ty = CTyInt (),
      id = Some n,
      -- TODO: base output array size on related input array, rather than
      -- hard coded to be based on first parameter
      init = Some (CIExpr { expr = camlWosizeVal_ (head params).1 })
    } in

    -- Define output variable and allocate its memory
    let outDecl = CSDef {
      ty = CTyPtr { ty = valueTy_ },
      id = Some cudaOut,
      init = None ()
    } in
    let cudaOutAlloc = cudaMalloc_ [
      CEUnOp { op = COAddrOf (), arg = cudavar_ cudaOut },
      CEBinOp {
        op = COMul (),
        lhs = cudavar_ n,
        rhs = CESizeOfType { ty = valueTy_ }
      }
    ] in

    -- For each argument, move data host -> device
    let moveToDevice = map allocAndCopyToDevice params in
    let cudaParams = map (lam p. p.0) moveToDevice in
    let cudaCopyStmts = join (map (lam p. p.1) moveToDevice) in
    
    let kernelName = nameWithKernelTail name in
    let kernelStmts = kernelCall kernelName cudaParams n in

    -- Allocate output array on host and copy data device -> host
    let outAlloc = CSExpr { expr = CEBinOp {
      op = COAssign (),
      lhs = cudavar_ out,
      -- TODO: select different tag based on output type
      rhs = camlAlloc_ n _doubleArrayTag
    }} in
    let deviceToHostMemcpy = cudaMemcpyD2H_ [
      camlOpVal_ out,
      cudavar_ cudaOut,
      CEBinOp {
        op = COMul (),
        lhs = camlWosizeVal_ out,
        rhs = CESizeOfType { ty = valueTy_ }
      }
    ] in

    -- Free GPU memory of all variables that were copied there
    let freeStmts = map cudaFree_ cudaParams in

    -- Return to OCaml
    let returnStmt = camlReturn_ out in

    let cparams = map (lam p. (valueTy_, p.1)) params in
    let stmts = join [
      camlParam, [camlLocal, nDecl, outDecl, cudaOutAlloc],
      cudaCopyStmts,
      kernelStmts,
      [outAlloc, deviceToHostMemcpy],
      freeStmts,
      [returnStmt]
    ] in
    CudaFun {
      ret = CTyIdent { id = _value },
      id = name,
      params = cparams,
      body = stmts,
      annot = [CudaExternC ()]
    }
end

mexpr

use CudaWrapperGenerate in
use CudaPrettyPrint in

let templateName = nameSym "fold" in
let params = [
  (TySeq { ty = TyFloat ()}, nameSym "a")
] in
let outty = TyFloat () in
let program = CPProg {
  includes = ["<caml/alloc.h>", "<caml/memory.h>", "<caml/mlvalues.h>"],
  tops = [cudaTemplate templateName params]
} in

let _ = printLn (printCProg [] program) in

utest length (printCProg [] program) with 0 using geqi in

()

