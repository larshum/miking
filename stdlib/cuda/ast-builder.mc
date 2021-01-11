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
