include "cuda/ast.mc"
include "cuda/pprint.mc"
include "mexpr/ast.mc"

let cudaMalloc = nameSym "cudaMalloc"
let cudaMemcpy = nameSym "cudaMemcpy"
let cudaFree = nameSym "cudaFree"
let cudaDeviceGetAttribute = nameSym "cudaDeviceGetAttribute"
let cudaDevAttrMaxThreadsPerBlock = nameSym "cudaDevAttrMaxThreadsPerBlock"
let cudaMemcpyHostToDevice = nameSym "cudaMemcpyHostToDevice"
let cudaMemcpyDeviceToHost = nameSym "cudaMemcpyDeviceToHost"
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
    let sz = CEBinOp {
      op = COMul (),
      lhs = CEApp {
        fun = wosizeVal,
        args = [CEVar { id = paramName }]
      },
      rhs = CESizeOfType { ty = valuety }
    } in
    (cudaParam, [
      CSDef {
        ty = CTyPtr { ty = valuety },
        id = Some cudaParam,
        init = None ()
      },
      CSExpr { expr = CEApp {
        fun = cudaMalloc,
        args = [CEUnOp { op = COAddrOf (), arg = CEVar { id = cudaParam } }, sz]
      }},
      CSExpr { expr = CEApp {
        fun = cudaMemcpy,
        args = [
          CEVar { id = cudaParam },
          CEApp { fun = opVal, args = [CEVar { id = paramName}] },
          CEBinOp {
            op = COMul (),
            lhs = CEApp { fun = wosizeVal, args = [CEVar { id = paramName }] },
            rhs = CESizeOfType { ty = valuety }
          },
          CEVar { id = cudaMemcpyHostToDevice }
        ]
      }}
    ])

  sem freeStatement =
  | cudaParam ->
    CSExpr { expr = CEApp { fun = cudaFree, args = [CEVar { id = cudaParam }] } }

  sem kernelCall (name: Name) (params: [Name]) =
  | n ->
    let tpb = nameSym "tpb" in
    let blocks = nameSym "blocks" in
    let tpbDecl = CSDef {
      ty = CTyInt (),
      id = Some tpb,
      init = None ()
    } in
    let tpbMaxThreadsCall = CSExpr { expr = CEApp {
      fun = cudaDeviceGetAttribute,
      args = [
        CEUnOp { op = COAddrOf (), arg = CEVar { id = tpb } },
        CEVar { id = cudaDevAttrMaxThreadsPerBlock },
        CEInt { i = 0 }
      ]
    }} in
    let blocksDef = CSDef {
      ty = CTyInt (),
      id = Some blocks,
      init = Some (CIExpr { expr = CEBinOp {
        op = CODiv (),
        lhs = CEBinOp {
          op = COSub (),
          lhs = CEBinOp {
            op = COAdd (),
            lhs = CEVar { id = n },
            rhs = CEVar { id = tpb }
          },
          rhs = CEInt { i = 1 }
        },
        rhs = CEVar { id = tpb }
      }})
    } in
    let kernelFunctionCall = CSExpr { expr = CudaEApp {
      fun = name,
      args = map (lam param. CEVar { id = param }) params,
      blocks = CEVar { id = blocks },
      tpb = CEVar { id = tpb }
    }} in
    [tpbDecl, tpbMaxThreadsCall, blocksDef, kernelFunctionCall]

  sem cudaTemplate (name: Name) =
  | params ->
    let out = nameSym "out" in
    let cudaOut = nameSym "cuda_out" in

    -- TODO: this approach only works for up to 5 parameters
    let paramFunName = join ["CAMLparam", int2string (length params)] in
    let argNames = map (lam p. CEVar{ id = p.1 }) params in
    let camlParam = CSExpr { expr = CEApp {
      fun = nameSym paramFunName,
      args = argNames
    }} in
    let camlLocal = CSExpr { expr = CEApp {
      fun = nameSym "CAMLlocal1",
      args = [CEVar { id = out }]
    }} in

    -- Define helper variable n which stores the size of the output array.
    let n = nameSym "n" in
    let nDecl = CSDef {
      ty = CTyInt (),
      id = Some n,
      init = Some (CIExpr { expr = CEApp {
        fun = wosizeVal,
        -- TODO: base output array size on related input array, rather than
        -- hard coded to be based on first parameter
        args = [CEVar { id = (head params).1 }]
      }})
    } in

    -- Define output variable and allocate its memory
    let outDecl = CSDef {
      ty = CTyPtr { ty = valuety },
      id = Some cudaOut,
      init = None ()
    } in
    let cudaOutAlloc = CSExpr { expr = CEApp {
      fun = cudaMalloc,
      args = [
        CEUnOp { op = COAddrOf (), arg = CEVar { id = cudaOut } },
        CEBinOp {
          op = COMul (),
          lhs = CEVar { id = n },
          rhs = CESizeOfType { ty = valuety }
        }
      ]
    }} in

    -- For each argument, move data host -> device
    let moveToDevice = map allocAndCopyToDevice params in
    let cudaParams = map (lam p. p.0) moveToDevice in
    let cudaCopyStmts = join (map (lam p. p.1) moveToDevice) in
    
    let kernelName = nameWithKernelTail name in
    let kernelStmts = kernelCall kernelName cudaParams n in

    -- Allocate output array on host and copy data device -> host
    let outAlloc = CSExpr { expr = CEBinOp {
      op = COAssign (),
      lhs = CEVar { id = out },
      rhs = CEApp {
        fun = camlAlloc,
        args = [
          CEVar { id = n },
          CEVar { id = doubleArrayTag } -- TODO: select different tag based on output type
        ]
      }
    }} in
    let deviceToHostMemcpy = CSExpr { expr = CEApp {
      fun = cudaMemcpy,
      args = [
        CEApp {
          fun = opVal,
          args = [CEVar { id = out }]
        },
        CEVar { id = cudaOut },
        CEBinOp {
          op = COMul (),
          lhs = CEApp { fun = wosizeVal, args = [CEVar { id = out }] },
          rhs = CESizeOfType { ty = valuety }
        },
        CEVar { id = cudaMemcpyDeviceToHost }
      ]
    }} in

    -- Free GPU memory of all variables that were copied there
    let freeStmts = map freeStatement cudaParams in

    -- Return to OCaml
    let camlReturn = CSExpr { expr = CEApp {
      fun = camlReturn,
      args = [CEVar { id = out }]
    }} in

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

