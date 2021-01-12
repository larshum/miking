include "cuda/ast.mc"

let _cudaMalloc = nameSym "cudaMalloc"
let _cudaMemcpy = nameSym "cudaMemcpy"
let _cudaFree = nameSym "cudaFree"
let _cudaDeviceGetAttribute = nameSym "cudaDeviceGetAttribute"

let _cudaMemcpyHostToDevice = nameSym "cudaMemcpyHostToDevice"
let _cudaMemcpyDeviceToHost = nameSym "cudaMemcpyDeviceToHost"
let _cudaDevAttrMaxThreadsPerBlock = nameSym "cudaDevAttrMaxThreadsPerBlock"

let cudavar_ = use CudaAst in
  lam n.
  CEVar { id = n }

let cudaAppStmt_ = use CudaAst in
  lam fun. lam args.
  CSExpr { expr = CEApp { fun = fun, args = args } }

let cudaMalloc_ = use CudaAst in
  lam args.
  cudaAppStmt_ _cudaMalloc args

let cudaMemcpyH2D_ = use CudaAst in
  lam args.
  cudaAppStmt_ _cudaMemcpy (join [args, [cudavar_ _cudaMemcpyHostToDevice]])

let cudaMemcpyD2H_ = use CudaAst in
  lam args.
  cudaAppStmt_ _cudaMemcpy (join [args, [cudavar_ _cudaMemcpyDeviceToHost]])

let cudaFree_ = use CudaAst in
  lam param.
  cudaAppStmt_ _cudaFree [cudavar_ param]

let cudaGetMaxThreadsPerBlock_ = use CudaAst in
  lam tpb.
  cudaAppStmt_ _cudaDeviceGetAttribute [
    CEUnOp { op = COAddrOf (), arg = cudavar_ tpb },
    cudavar_ _cudaDevAttrMaxThreadsPerBlock,
    CEInt { i = 0 }
  ]

-- OCaml related definitions

let _camlParam = [
  nameSym "CAMLparam0",
  nameSym "CAMLparam1",
  nameSym "CAMLparam2",
  nameSym "CAMLparam3",
  nameSym "CAMLparam4",
  nameSym "CAMLparam5"
]

let _camlXparam = [
  null,
  nameSym "CAMLxparam1",
  nameSym "CAMLxparam2",
  nameSym "CAMLxparam3",
  nameSym "CAMLxparam4",
  nameSym "CAMLxparam5"
]

let camlParams_ = use CudaAst in
  lam paramNames.
  let n = length paramNames in
  if and (geqi n 0) (leqi n 5) then
    cudaAppStmt_ (get _camlParam n) paramNames
  else
    error "CAMLparam is only defined for between zero and five arguments"

let camlXparams_ = use CudaAst in
  lam paramNames.
  let n = length paramNames in
  if and (gti n 0) (leqi n 5) then
    cudaAppStmt_ (get _camlXparam n) paramNames
  else
    error "CAMLxparam is only defined for between one and five arguments"
