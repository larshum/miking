include "c/compile.mc"
include "cuda/ast.mc"
include "cuda/pmexpr-ast.mc"
include "pmexpr/utils.mc"

let _cudaMalloc = nameNoSym "cudaMalloc"
let _cudaMemcpy = nameNoSym "cudaMemcpy"
let _cudaMemcpyDeviceToHost = nameNoSym "cudaMemcpyDeviceToHost"
let _cudaMemcpyHostToDevice = nameNoSym "cudaMemcpyHostToDevice"
let _cudaFree = nameNoSym "cudaFree"
let _malloc = nameNoSym "malloc"
let _free = nameNoSym "free"

let cudaIncludes = concat cIncludes []

lang CudaCompileCopy = MExprCCompileAlloc + CudaPMExprAst + CudaAst
  sem _compileCopy (env : CompileCEnv) (dst : CExpr) (arg : CExpr) (ty : CType) =
  | Cpu _ -> _compileCopyToCpu env dst arg ty
  | Gpu _ -> _compileCopyToGpu env dst arg ty

  sem _compileCopyToCpu (env : CompileCEnv) (dst : CExpr) (arg : CExpr) =
  | ty & (TySeq {ty = elemType}) ->
      let seqType = compileType env ty in
      let copySeqStmt = CSExpr {expr = CEApp {
        fun = _cudaMemcpy, args = [
          CEUnOp {op = COAddrOf (), arg = dst}, arg,
          CESizeOfType {ty = seqType}, CEVar {id = _cudaMemcpyDeviceToHost}]}} in
      let elemType = compileType env elemType in
      let sizeExpr = CEBinOp {
        op = COMul (),
        lhs = CEMember {lhs = dst, id = _seqLenKey},
        rhs = CESizeOfType {ty = elemType}} in
      let tempType = CTyPtr {ty = elemType} in
      let tempId = nameSym "t" in
      let tempAllocStmt = CSDef {
        ty = CTyPtr {ty = elemType}, id = Some tempId,
        init = Some (CIExpr {expr = CECast {
          ty = tempType,
          rhs = CEApp {
            fun = _malloc,
            args = [sizeExpr]}}})} in
      let copyDataStmt = CSExpr {expr = CEApp {
        fun = _cudaMemcpy,
        args = [
          CEVar {id = tempId},
          CEMember {lhs = dst, id = _seqKey},
          sizeExpr, CEVar {id = _cudaMemcpyDeviceToHost}]}} in
      let outTmpStmt = CSExpr {expr = CEBinOp {
        op = COAssign (),
        lhs = CEMember {lhs = dst, id = _seqKey},
        rhs = CEVar {id = tempId}}} in
      [copySeqStmt, tempAllocStmt, copyDataStmt, outTmpStmt]
  | TyTensor {ty = elemType} ->
    -- NOTE(larshum, 2022-03-16): Tensor memory operations are handled at a
    -- later stage in the compiler.
    let tensorDataType = CTyPtr {ty = compileType env elemType} in
    [CSTensorDataCopyCpu {src = arg, dst = dst, dataTy = tensorDataType}]
  | TyInt _ | TyChar _ | TyFloat _ | TyBool _ ->
    [CSExpr {expr = CEBinOp {op = COAssign (), lhs = dst, rhs = arg}}]
  | TyRecord t ->
    mapFoldWithKey
      (lam acc : [CStmt]. lam key : SID. lam ty : Type.
        let ty = _unwrapType env.typeEnv ty in
        let fieldId = nameNoSym (sidToString key) in
        let dstField = CEMember {lhs = dst, id = fieldId} in
        let argField = CEMember {lhs = arg, id = fieldId} in
        concat acc (_compileCopyToCpu env dstField argField ty))
      [] t.fields
  | ty ->
    use MExprPrettyPrint in
    let tystr = type2str ty in
    error (concat "Copying GPU -> CPU not implemented for type " tystr)

  sem _compileCopyToGpu (env : CompileCEnv) (dst : CExpr) (arg : CExpr) =
  | ty & (TySeq {ty = elemType}) ->
    let seqType = compileType env ty in
    let elemType = compileType env elemType in
    let tempId = nameSym "t" in
    let tempSeqDeclStmt = CSDef {ty = seqType, id = Some tempId, init = None ()} in
    let tempSetLenStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = CEMember {lhs = CEVar {id = tempId}, id = _seqLenKey},
      rhs = CEMember {lhs = arg, id = _seqLenKey}}} in
    let sizeExpr = CEBinOp {
      op = COMul (),
      lhs = CEMember {lhs = CEVar {id = tempId}, id = _seqLenKey},
      rhs = CESizeOfType {ty = elemType}} in
    let cudaMallocDataStmt = CSExpr {expr = CEApp {
      fun = _cudaMalloc,
      args = [
        CEUnOp {op = COAddrOf (),
                arg = CEMember {lhs = CEVar {id = tempId}, id = _seqKey}},
        sizeExpr]}} in
    let cudaMemcpyDataStmt = CSExpr {expr = CEApp {
      fun = _cudaMemcpy,
      args = [
        CEMember {lhs = CEVar {id = tempId}, id = _seqKey},
        CEMember {lhs = arg, id = _seqKey},
        sizeExpr, CEVar {id = _cudaMemcpyHostToDevice}]}} in
    let cudaMallocSeqStmt = CSExpr {expr = CEApp {
      fun = _cudaMalloc,
      args = [
        CEUnOp {op = COAddrOf (), arg = dst},
        CESizeOfType {ty = seqType}]}} in
    let cudaMemcpySeqStmt = CSExpr {expr = CEApp {
      fun = _cudaMemcpy,
      args = [
        dst,
        CEUnOp {op = COAddrOf (), arg = CEVar {id = tempId}},
        CESizeOfType {ty = seqType},
        CEVar {id = _cudaMemcpyHostToDevice}]}} in
    [ tempSeqDeclStmt, tempSetLenStmt, cudaMallocDataStmt, cudaMemcpyDataStmt
    , cudaMallocSeqStmt, cudaMemcpySeqStmt ]
  | TyTensor {ty = elemType} ->
    -- NOTE(larshum, 2022-03-16): Tensor memory operations are handled at a
    -- later stage in the compiler.
    let tensorDataType = CTyPtr {ty = compileType env elemType} in
    [CSTensorDataCopyGpu {src = arg, dst = dst, dataTy = tensorDataType}]
  | TyRecord t ->
    mapFoldWithKey
      (lam acc : [CStmt]. lam key : SID. lam ty : Type.
        let ty = _unwrapType env.typeEnv ty in
        let fieldId = nameNoSym (sidToString key) in
        let dstField = CEMember {lhs = dst, id = fieldId} in
        let argField = CEMember {lhs = arg, id = fieldId} in
        concat acc (_compileCopyToGpu env dstField argField ty))
      [] t.fields
  | TyInt _ | TyFloat _ | TyChar _ | TyBool _ ->
    [CSExpr {expr = CEBinOp { op = COAssign (), lhs = dst, rhs = arg}}]
  | ty ->
    use MExprPrettyPrint in
    let tystr = type2str ty in
    error (concat "Copying CPU -> GPU not implemented for type " tystr)
end

lang CudaCompileFree = MExprCCompileAlloc + CudaPMExprAst + CudaAst
  sem _compileFree (env : CompileCEnv) (arg : CExpr) (tyArg : Type) =
  | Cpu _ -> _compileFreeCpu env arg tyArg
  | Gpu _ -> _compileFreeGpu env arg tyArg

  sem _compileFreeCpu (env : CompileCEnv) (arg : CExpr) =
  | TySeq _ ->
    [CSExpr {expr = CEApp {
      fun = _free,
      args = [CEMember {lhs = arg, id = _seqKey}]}}]
  | TyTensor _ | TyInt _ | TyFloat _ | TyChar _ | TyBool _ -> []
  | TyRecord t ->
    mapFoldWithKey
      (lam acc : [CStmt]. lam key : SID. lam ty : Type.
        let ty = _unwrapType env.typeEnv ty in
        let fieldId = nameNoSym (sidToString key) in
        let fieldArg = CEMember {lhs = arg, id = fieldId} in
        concat acc (_compileFreeCpu env fieldArg ty))
      [] t.fields
  | ty ->
    use MExprPrettyPrint in
    let tystr = type2str ty in
    error (concat "Freeing CPU memory not implemented for type " tystr)

  sem _compileFreeGpu (env : CompileCEnv) (arg : CExpr) =
  | ty & (TySeq _) ->
    let tempId = nameSym "t" in
    let temp = CEVar {id = tempId} in
    let tempDeclStmt = CSDef {ty = ty, id = Some tempId, init = None ()} in
    let cudaMemcpySeqStmt = CSExpr {expr = CEApp {
      fun = _cudaMemcpy,
      args = [
        CEUnOp {op = COAddrOf (), arg = CEVar {id = tempId}}, arg,
        CESizeOfType {ty = ty},
        CEVar {id = _cudaMemcpyDeviceToHost}]}} in
    let cudaFreeGpuDataStmt = CSExpr {expr = CEApp {
      fun = _cudaFree, args = [CEMember {lhs = temp, id = _seqKey}]}} in
    let cudaFreeGpuSeqStmt = CSExpr {expr = CEApp {
      fun = _cudaFree, args = [arg]}} in
    [tempDeclStmt, cudaMemcpySeqStmt, cudaFreeGpuDataStmt, cudaFreeGpuSeqStmt]
  | TyTensor _ ->
    -- NOTE(larshum, 2022-03-16): Tensor memory operations are handled at a
    -- later stage in the compiler.
    [CSTensorDataFreeGpu {arg = arg}]
  | TyRecord t ->
    mapFoldWithKey
      (lam acc : [CStmt]. lam key : SID. lam ty : Type.
        let ty = _unwrapType env.typeEnv ty in
        let fieldId = nameNoSym (sidToString key) in
        let fieldArg = CEMember {lhs = arg, id = fieldId} in
        concat acc (_compileFreeGpu env fieldArg ty))
      [] t.fields
  | TyInt _ | TyFloat _ | TyChar _ | TyBool _ -> []
  | ty ->
    use MExprPrettyPrint in
    let tystr = type2str ty in
    error (concat "Freeing GPU memory not implemented for type " tystr)
end

lang CudaCompile = CudaCompileCopy + CudaCompileFree
  sem compileExpr (env : CompileCEnv) =
  | TmSeqMap t -> error "not supported yet";
    CESeqMap {
      f = compileExpr env t.f, s = compileExpr env t.s,
      sTy = compileType env (tyTm t.s), ty = compileType env t.ty}
  | TmSeqFoldl t -> error "not supported yet";
    CESeqFoldl {
      f = compileExpr env t.f, acc = compileExpr env t.acc,
      s = compileExpr env t.s, sTy = compileType env (tyTm t.s),
      ty = compileType env t.ty}
  | TmTensorSliceExn t ->
    CETensorSliceExn {
      t = compileExpr env t.t, slice = compileExpr env t.slice,
      ty = compileType env t.ty}
  | TmTensorSubExn t ->
    CETensorSubExn {
      t = compileExpr env t.t, ofs = compileExpr env t.ofs,
      len = compileExpr env t.len, ty = compileType env t.ty}
  | TmMapKernel t -> error "not supported yet";
    -- TODO(larshum, 2022-02-08): Add a way to control the value of the
    -- 'opsPerThread' argument from the CUDA PMExpr AST.
    CEMapKernel {
      f = compileExpr env t.f, s = compileExpr env t.s,
      sTy = compileType env (tyTm t.s), ty = compileType env t.ty,
      opsPerThread = 10}
  | TmReduceKernel t -> error "not implemented yet"
  | op & (TmLoop t | TmParallelLoop t | TmLoopKernel t) ->
    let argTypes =
      match t.f with TmVar _ then []
      else match t.f with TmApp _ then
        match collectAppArguments t.f with (_, args) in
        map (lam arg. compileType env (tyTm arg)) args
      else
        infoErrorExit t.info "Unsupported function type"
    in
    -- NOTE(larshum, 2022-03-08): Parallel loops that were not promoted to a
    -- kernel are compiled to sequential loops.
    match op with TmLoopKernel _ then
      CELoopKernel {
        n = compileExpr env t.n, f = compileExpr env t.f, argTypes = argTypes,
        opsPerThread = 10}
    else
      CESeqLoop {
        n = compileExpr env t.n, f = compileExpr env t.f, argTypes = argTypes}

  sem compileStmt (env : CompileCEnv) (res : Result) =
  | TmCopy {arg = arg, toMem = toMem, ty = ty} ->
    let arg = CEVar {id = arg} in
    match res with RReturn _ then
      match ty with TyRecord {labels = []} then (env, [])
      else
        let dstId = nameSym "temp" in
        let dst = CEVar {id = dstId} in
        let ty = _unwrapType env.typeEnv ty in
        let copyStmts = _compileCopy env dst arg ty toMem in
        (env, snoc copyStmts (CSRet {val = Some dst}))
    else match res with RNone _ then (env, [])
    else match res with RIdent dstId in
      let dst = CEVar {id = dstId} in
      let ty = _unwrapType env.typeEnv ty in
      (env, _compileCopy env dst arg ty toMem)
  | TmFree {arg = arg, tyArg = tyArg, mem = mem} ->
    let arg = CEVar {id = arg} in
    let tyArg = _unwrapType env.typeEnv tyArg in
    (env, _compileFree env arg tyArg mem)

end
