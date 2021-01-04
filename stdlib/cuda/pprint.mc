include "c/pprint.mc"
include "ast.mc"

lang CudaPrettyPrint = CudaAst + CPrettyPrint
    sem printCExpr (env: PprintEnv) =
    | CudaEApp { fun = fun, args = args, blocks = blocks, tpb = tpb } ->
      match pprintEnvGetStr env fun with (env,fun) then
        match mapAccumL printCExpr env args with (env,args) then
          match printCExpr env blocks with (env,blocksStr) then
            match printCExpr env tpb with (env, tpbStr) then
              (env, _par (join [fun, "<<<", blocksStr, ", ", tpbStr, ">>>(", (strJoin ", " args), ")"]))
            else never
          else never
        else never
      else never

    sem printCTop (indent: Int) (env: PprintEnv) =
    | CudaFun { ret = ret, id = id, params = params, body = body, annot = annot } ->
      let i = indent in
      let ii = pprintIncr indent in
      match pprintEnvGetStr env id with (env,id) then
        let f = lam env. lam t.
          match pprintEnvGetStr env t.1 with (env,t1) then
            printCDef env t.0 t1 (None ())
          else never
        in
        let annot = strJoin " " (map (printCudaAnnot env) annot) in
        match mapAccumL f env params with (env,params) then
          let params = join ["(", strJoin ", " params, ")"] in
          match printCType (join [id, params]) env ret with (env,ty) then
            match printCStmts ii env body with (env,body) then
              (env, join [annot, " ", ty, " {", pprintNewline ii, body, pprintNewline i, "}"])
            else never
          else never
        else never
      else never

    sem printCudaAnnot (env: PprintEnv) =
    | CudaExternC {} -> "extern \"C\""
    | CudaDevice  {} -> "__device__"
    | CudaGlobal  {} -> "__global__"
end

mexpr

use CudaPrettyPrint in

let addfname = nameSym "addf_cuda" in
let mulfname = nameSym "mulf_cuda" in
let kernelname = nameSym "axpy_kernel" in
let axpyname = nameSym "axpy_cuda" in

let camlParam3Name = nameSym "CAMLparam3" in
let camlLocal1Name = nameSym "CAMLlocal1" in
let wosizeValName = nameSym "Wosize_val" in
let doubleValName = nameSym "Double_val" in
let opValName = nameSym "Op_val" in
let camlAllocName = nameSym "caml_alloc" in
let doubleFieldName = nameSym "Double_field" in
let storeDoubleFieldName = nameSym "Store_double_field" in
let camlReturnName = nameSym "CAMLreturn" in
let doubleArrayTagName = nameSym "Double_array_tag" in

let cudaMallocName = nameSym "cudaMalloc" in
let cudaMemcpyName = nameSym "cudaMemcpy" in
let cudaFreeName = nameSym "cudaFree" in
let cudaMemcpyHostToDeviceName = nameSym "cudaMemcpyHostToDevice" in
let cudaMemcpyDeviceToHostName = nameSym "cudaMemcpyDeviceToHost" in
let cudaDeviceGetAttributeName = nameSym "cudaDeviceGetAttribute" in
let cudaDevAttrMaxThreadsPerBlockName = nameSym "cudaDevAttrMaxThreadsPerBlock" in
let threadIdx = CEMember { lhs = CEVar { id = nameSym "threadIdx" }, id = "x" } in
let blockIdx = CEMember { lhs = CEVar { id = nameSym "blockIdx" }, id = "x" } in
let blockDim = CEMember { lhs = CEVar { id = nameSym "blockDim" }, id = "x" } in

let addfa = nameSym "a" in
let addfb = nameSym "b" in
let addfbody = [
  CSRet { val = Some (CEBinOp {
    op = COAdd {},
    lhs = CEVar { id = addfa },
    rhs = CEVar { id = addfb }
  })}
] in

let addfdef = CudaFun {
  ret = CTyDouble {},
  id = addfname,
  params = [(CTyDouble {}, addfa), (CTyDouble {}, addfb)],
  body = addfbody,
  annot = [CudaDevice {}]
} in

let mulfa = nameSym "a" in
let mulfb = nameSym "b" in
let mulfbody = [
  CSRet { val = Some (CEBinOp {
    op = COMul {},
    lhs = CEVar { id = mulfa },
    rhs = CEVar { id = mulfb }
  }) }
] in

let mulfdef = CudaFun {
  ret = CTyDouble {},
  id = mulfname,
  params = [(CTyDouble {}, mulfa), (CTyDouble {}, mulfb)],
  body = mulfbody,
  annot = [CudaDevice {}]
} in

let valuety = CTyIdent { id = nameSym "value" } in
let valueptr = CTyPtr { ty = valuety } in

let kernela = nameSym "a" in
let kernelx = nameSym "x" in
let kernely = nameSym "y" in
let kernelout = nameSym "out" in
let kerneln = nameSym "n" in
let kernelidx = nameSym "idx" in

let kernelbody = [
  CSDef {
    ty = CTyInt {},
    id = Some kernelidx,
    init = Some (CIExpr {
      expr = CEBinOp {
        op = COAdd {},
        lhs = threadIdx,
        rhs = CEBinOp {
          op = COMul {},
          lhs = blockIdx,
          rhs = blockDim
        }
      }
    })
  },
  CSIf {
    cond = CEBinOp {
      op = COLt {},
      lhs = CEVar { id = kernelidx },
      rhs = CEVar { id = kerneln }
    },
    thn = [
      CSExpr { expr = CEApp {
        fun = storeDoubleFieldName,
        args = [
          CEVar { id = kernelout },
          CEVar { id = kernelidx },
          CEApp {
            fun = addfname,
            args = [
              CEApp {
                fun = mulfname,
                args = [
                  CEVar { id = kernela },
                  CEApp {
                    fun = doubleFieldName,
                    args = [
                      CEVar { id = kernelx },
                      CEVar { id = kernelidx }
                    ]
                  }
                ]
              },
              CEApp {
                fun = doubleFieldName,
                args = [
                  CEVar { id = kernely },
                  CEVar { id = kernelidx }
                ]
              }
            ]
          }
        ]
      }}
    ],
    els = []
  }
] in

let kerneldef = CudaFun {
  ret = CTyVoid {},
  id = kernelname,
  params = [
    (CTyDouble {}, kernela),
    (valueptr, kernelx),
    (valueptr, kernely),
    (valueptr, kernelout),
    (CTyInt {}, kerneln)
  ],
  body = kernelbody,
  annot = [CudaGlobal {}]
} in

let axpyatmp = nameSym "atmp" in
let axpyx = nameSym "x" in
let axpyy = nameSym "y" in
let axpyoutarr = nameSym "outarr" in
let axpyn = nameSym "n" in
let axpya = nameSym "a" in
let axpytpb = nameSym "tpb" in
let axpyblocks = nameSym "blocks" in
let axpycudax = nameSym "cuda_x" in
let axpycuday = nameSym "cuda_y" in
let axpycudaoutarr = nameSym "cuda_outarr" in

let axpybody = [
  CSExpr { expr = CEApp {
    fun = camlParam3Name,
    args = [CEVar { id = axpyatmp }, CEVar { id = axpyx }, CEVar { id = axpyy }]
  }},
  CSExpr { expr = CEApp {
    fun = camlLocal1Name,
    args = [CEVar { id = axpyoutarr }]
  }},
  CSDef {
    ty = CTyInt {},
    id = Some axpyn,
    init = Some (CIExpr { expr = CEApp {
      fun = wosizeValName,
      args = [CEVar { id = axpyx }]
    }})
  },
  CSDef {
    ty = CTyDouble {},
    id = Some axpya,
    init = Some (CIExpr { expr = CEApp {
      fun = doubleValName,
      args = [CEVar { id = axpyatmp }]
    }})
  },

  -- Memory allocation statements
  CSDef {
    ty = valueptr,
    id = Some axpycudaoutarr,
    init = None ()
  },
  CSDef {
    ty = valueptr,
    id = Some axpycudax,
    init = None ()
  },
  CSDef {
    ty = valueptr,
    id = Some axpycuday,
    init = None ()
  },
  CSExpr { expr = CEApp {
    fun = cudaMallocName,
    args = [
      CEUnOp { op = COAddrOf {}, arg = CEVar { id = axpycudaoutarr }},
      CEBinOp {
        op = COMul {},
        lhs = CEVar { id = axpyn },
        rhs = CESizeOfType { ty = valuety }
      }
    ]
  }},
  CSExpr { expr = CEApp {
    fun = cudaMallocName,
    args = [
      CEUnOp { op = COAddrOf {}, arg = CEVar { id = axpycudax }},
      CEBinOp {
        op = COMul {},
        lhs = CEApp {
          fun = wosizeValName,
          args = [CEVar { id = axpyx }]
        },
        rhs = CESizeOfType { ty = valuety }
      }
    ]
  }},
  CSExpr { expr = CEApp {
    fun = cudaMallocName,
    args = [
      CEUnOp { op = COAddrOf {}, arg = CEVar { id = axpycuday }},
      CEBinOp {
        op = COMul {},
        lhs = CEApp {
          fun = wosizeValName,
          args = [CEVar { id = axpyy }]
        },
        rhs = CESizeOfType { ty = valuety }
      }
    ]
  }},
  CSExpr { expr = CEApp {
    fun = cudaMemcpyName,
    args = [
      CEVar { id = axpycudax },
      CEApp {
        fun = opValName,
        args = [CEVar { id = axpyx }]
      },
      CEBinOp {
        op = COMul {},
        lhs = CEApp {
          fun = wosizeValName,
          args = [CEVar { id = axpyx }]
        },
        rhs = CESizeOfType { ty = valuety }
      },
      CEVar { id = cudaMemcpyHostToDeviceName }
    ]
  }},
  CSExpr { expr = CEApp {
    fun = cudaMemcpyName,
    args = [
      CEVar { id = axpycuday },
      CEApp {
        fun = opValName,
        args = [CEVar { id = axpyy }]
      },
      CEBinOp {
        op = COMul {},
        lhs = CEApp {
          fun = wosizeValName,
          args = [CEVar { id = axpyy }]
        },
        rhs = CESizeOfType { ty = valuety }
      },
      CEVar { id = cudaMemcpyHostToDeviceName }
    ]
  }},

  -- Kernel call
  CSDef {
    ty = CTyInt {},
    id = Some axpytpb,
    init = None ()
  },
  CSExpr { expr = CEApp {
    fun = cudaDeviceGetAttributeName,
    args = [
      CEUnOp { op = COAddrOf {}, arg = CEVar { id = axpytpb } },
      CEVar { id = cudaDevAttrMaxThreadsPerBlockName },
      CEInt { i = 0 }
    ]
  }},
  CSDef {
    ty = CTyInt {},
    id = Some axpyblocks,
    init = Some (CIExpr { expr = CEBinOp {
      op = CODiv {},
      lhs = CEBinOp {
        op = COSub {},
        lhs = CEBinOp {
          op = COAdd {},
          lhs = CEVar { id = axpyn },
          rhs = CEVar { id = axpytpb }
        },
        rhs = CEInt { i = 1 }
      },
      rhs = CEVar { id = axpytpb }
    }})
  },
  CSExpr { expr = CudaEApp {
    fun = kernelname,
    args = [
      CEVar { id = axpya },
      CEVar { id = axpycudax },
      CEVar { id = axpycuday },
      CEVar { id = axpycudaoutarr },
      CEVar { id = axpyn }
    ],
    blocks = CEVar { id = axpyblocks },
    tpb = CEVar { id = axpytpb }
  }},
  CSExpr { expr = CEBinOp {
    op = COAssign {},
    lhs = CEVar { id = axpyoutarr },
    rhs = CEApp {
      fun = camlAllocName,
      args = [
        CEVar { id = axpyn },
        CEVar { id = doubleArrayTagName }
      ]
    }
  }},

  -- Device to host copy and memory deallocation
  CSExpr { expr = CEApp {
    fun = cudaMemcpyName,
    args = [
      CEApp {
        fun = opValName,
        args = [CEVar { id = axpyoutarr }]
      },
      CEVar { id = axpycudaoutarr },
      CEBinOp {
        op = COMul {},
        lhs = CEApp {
          fun = wosizeValName,
          args = [CEVar { id = axpyoutarr }]
        },
        rhs = CESizeOfType { ty = valuety }
      },
      CEVar { id = cudaMemcpyDeviceToHostName }
    ]
  }},
  CSExpr { expr = CEApp {
    fun = cudaFreeName,
    args = [CEVar { id = axpycudax }]
  }},
  CSExpr { expr = CEApp {
    fun = cudaFreeName,
    args = [CEVar { id = axpycuday }]
  }},
  CSExpr { expr = CEApp {
    fun = cudaFreeName,
    args = [CEVar { id = axpycudaoutarr }]
  }},

  -- OCAML return statement
  CSExpr { expr = CEApp {
    fun = camlReturnName,
    args = [CEVar { id = axpyoutarr }]
  }}
] in

let axpydef = CudaFun {
  ret = valuety,
  id = axpyname,
  params = [
    (valuety, axpyatmp),
    (valuety, axpyx),
    (valuety, axpyy)
  ],
  body = axpybody,
  annot = [CudaExternC {}]
} in

let prog = CPProg {
  includes = ["<caml/alloc.h>", "<caml/memory.h>", "<caml/mlvalues.h>"],
  tops = [addfdef, mulfdef, kerneldef, axpydef]
} in

let _ = printLn (printCProg [] prog) in

utest length (printCProg [] prog) with 0 using geqi in

()
