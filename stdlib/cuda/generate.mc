include "cuda/ast.mc"
include "cuda/ast-builder.mc"
include "cuda/pprint.mc"
include "mexpr/ast.mc"

let wosizeVal = nameSym "Wosize_val"
let opVal = nameSym "Op_val"
let value = nameSym "value"
let camlAlloc = nameSym "caml_alloc"
let doubleArrayTag = nameSym "Double_array_tag"
let camlReturn = nameSym "CAMLreturn"
let valuety = use CudaAst in
  CTyIdent { id = value }

let nameWithCudaTail = lam name.
  nameSym (join [nameGetStr name, "_cuda"])

let nameWithKernelTail = lam name.
  nameSym (join [nameGetStr name, "_kernel"])

lang CudaKernelGenerate = CudaAst + MExprAst
  -- TODO: implement copying for non-array types
  sem allocAndCopyToDevice =
  | (paramTy, paramName) ->
    let cudaParam = nameWithCudaTail paramName in
    (cudaParam, [
      CSDef {
        ty = CTyPtr { ty = valuety },
        id = Some cudaParam,
        init = None ()
      },
      cudaMalloc_ [
        CEUnOp { op = COAddrOf (), arg = cudavar_ cudaParam },
        CEBinOp {
          op = COMul (),
          lhs = CEApp {
            fun = wosizeVal,
            args = [cudavar_ paramName]
          },
          rhs = CESizeOfType { ty = valuety }
        }
      ],
      cudaMemcpyH2D_ [
        cudavar_ cudaParam,
        CEApp { fun = opVal, args = [cudavar_ paramName] },
        CEBinOp {
          op = COMul (),
          lhs = CEApp { fun = wosizeVal, args = [cudavar_ paramName] },
          rhs = CESizeOfType { ty = valuety }
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

    -- TODO: this approach only works for up to 5 parameters
    let paramFunName = join ["CAMLparam", int2string (length params)] in
    let argNames = map (lam p. cudavar_ p.1) params in
    let camlParam = cudaAppStmt_ (nameSym paramFunName) argNames in
    let camlLocal = cudaAppStmt_ (nameSym "CAMLlocal1") [cudavar_ out] in

    -- Define helper variable n which stores the size of the output array.
    let n = nameSym "n" in
    let nDecl = CSDef {
      ty = CTyInt (),
      id = Some n,
      init = Some (CIExpr { expr = CEApp {
        fun = wosizeVal,
        -- TODO: base output array size on related input array, rather than
        -- hard coded to be based on first parameter
        args = [cudavar_ (head params).1]
      }})
    } in

    -- Define output variable and allocate its memory
    let outDecl = CSDef {
      ty = CTyPtr { ty = valuety },
      id = Some cudaOut,
      init = None ()
    } in
    let cudaOutAlloc = cudaMalloc_ [
      CEUnOp { op = COAddrOf (), arg = cudavar_ cudaOut },
      CEBinOp {
        op = COMul (),
        lhs = cudavar_ n,
        rhs = CESizeOfType { ty = valuety }
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
      rhs = CEApp {
        fun = camlAlloc,
        args = [
          cudavar_ n,
          cudavar_ doubleArrayTag -- TODO: select different tag based on output type
        ]
      }
    }} in
    let deviceToHostMemcpy = cudaMemcpyD2H_ [
      CEApp { fun = opVal, args = [cudavar_ out] },
      cudavar_ cudaOut,
      CEBinOp {
        op = COMul (),
        lhs = CEApp { fun = wosizeVal, args = [cudavar_ out] },
        rhs = CESizeOfType { ty = valuety }
      }
    ] in

    -- Free GPU memory of all variables that were copied there
    let freeStmts = map cudaFree_ cudaParams in

    -- Return to OCaml
    let camlReturn = cudaAppStmt_ camlReturn [cudavar_ out] in

    let cparams = map (lam p. (valuety, p.1)) params in
    let stmts = join [
      [camlParam, camlLocal, nDecl, outDecl, cudaOutAlloc],
      cudaCopyStmts,
      kernelStmts,
      [outAlloc, deviceToHostMemcpy],
      freeStmts,
      [camlReturn]
    ] in
    CudaFun {
      ret = CTyIdent { id = value },
      id = name,
      params = cparams,
      body = stmts,
      annot = [CudaExternC ()]
    }
end

mexpr

use CudaKernelGenerate in
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

