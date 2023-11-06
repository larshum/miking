include "cuda/ast.mc"
include "cuda/compile.mc"
include "mexpr/ast.mc"
include "mexpr/cmp.mc"
include "mexpr/record.mc"
include "pmexpr/wrapper.mc"

let _tensorTypeId = nameNoSym "Tensor"
let _tensorDataId = nameNoSym "data"
let _tensorRankId = nameNoSym "rank"
let _tensorDimsId = nameNoSym "dims"
let _tensorSizeId = nameNoSym "size"

let _cudaErrorCheckStmt =
  use CudaAst in
  CSExpr {expr = CEApp {
    fun = _CUDA_UTILS_CHECK_CUDA_ERROR,
    args = []
  }}

lang CudaCWrapperBase = PMExprCWrapper + CudaAst + MExprAst + CudaCompile
  syn CDataRepr =
  | CudaSeqRepr {ident : Name, data : CDataRepr, elemTy : CType, ty : CType}
  | CudaTensorRepr {ident : Name, data : CDataRepr, elemTy : CType, ty : CType}
  | CudaRecordRepr {ident : Name, labels : [SID], fields : [CDataRepr], ty : CType}
  | CudaDataTypeRepr {ident : Name, constrs : Map Name (CType, CDataRepr), ty : CType}
  | CudaBaseTypeRepr {ident : Name, ty : CType}

  syn TargetWrapperEnv =
  | CudaTargetEnv {
      -- Provides a mapping from the name of the C wrapper function to a CUDA
      -- wrapper function which handles interaction with CUDA kernels. Note
      -- that the name of the C wrapper function must be globally unique as it
      -- will be called from the OCaml code, while that of the CUDA wrapper
      -- does not, since it is called from a function stored in the same file.
      wrapperMap : Map Name Name,
      
      -- Reversed type environment from type lifting. Enables looking up the
      -- name of the replacement of a lifted type.
      revTypeEnv : Map Type Name,

      -- C compiler environment, used to compile MExpr types to the C
      -- equivalents.
      compileCEnv : CompileCEnv,

      -- Determines the maximum rank of a tensor. Larger values result in more
      -- memory usage per tensor.
      tensorMaxRank : Int}

  sem lookupTypeIdent : TargetWrapperEnv -> Type -> Option Type
  sem lookupTypeIdent env =
  | (TyRecord t) & tyrec ->
    if mapIsEmpty t.fields then None ()
    else
    match env with CudaTargetEnv cenv in
    let fields : Option [(SID, Type)] =
      optionMapM
        (lam f : (SID, Type).
          match f with (key, ty) in
          match lookupTypeIdent env ty with Some ty then
            Some (key, ty)
          else None ())
        (tyRecordOrderedFields tyrec) in
    match fields with Some fieldsSeq then
      let fields = mapFromSeq cmpSID fieldsSeq in
      let ty = TyRecord {t with fields = fields} in
      optionMap
        (lam id. TyCon {ident = id, info = t.info})
        (mapLookup ty cenv.revTypeEnv)
    else None ()
  | ty & (TySeq {ty = elemTy} | TyTensor {ty = elemTy}) ->
    let elemTy =
      match lookupTypeIdent env elemTy with Some ty then ty
      else elemTy in
    let ty =
      match ty with TySeq t then TySeq {t with ty = elemTy}
      else match ty with TyTensor t then TyTensor {t with ty = elemTy}
      else never in
    match env with CudaTargetEnv cenv in
    match mapLookup ty cenv.revTypeEnv with Some id then
      Some (TyCon {ident = id, info = infoTy ty})
    else None ()
  | ty & (TyVariant _) ->
    match env with CudaTargetEnv cenv in
    match mapLookup ty cenv.revTypeEnv with Some id then
      Some (TyCon {ident = id, info = infoTy ty})
    else None ()
  | TyAlias t -> lookupTypeIdent env t.content
  | ty -> Some ty

  sem getCudaType : TargetWrapperEnv -> Type -> CType
  sem getCudaType env =
  | ty & (TyRecord {fields = fields}) ->
    match env with CudaTargetEnv cenv in
    if mapIsEmpty fields then CTyVoid ()
    else match lookupTypeIdent env ty with Some (TyCon {ident = id}) then
      if any (nameEq id) cenv.compileCEnv.ptrTypes then
        CTyPtr {ty = CTyVar {id = id}}
      else CTyVar {id = id}
    else
      errorSingle [infoTy ty]
        "Reverse type lookup failed when generating CUDA wrapper code"
  | ty & (TySeq _ | TyTensor _ | TyVariant _) ->
    match env with CudaTargetEnv cenv in
    match lookupTypeIdent env ty with Some (TyCon {ident = id}) then
      if any (nameEq id) cenv.compileCEnv.ptrTypes then
        CTyPtr {ty = CTyVar {id = id}}
      else CTyVar {id = id}
    else
      errorSingle [infoTy ty]
        "Reverse type lookup failed when generating CUDA wrapper code"
  | ty ->
    match env with CudaTargetEnv cenv in
    compileType cenv.compileCEnv ty

  sem getOcamlTensorType : TargetWrapperEnv -> Type -> CType
  sem getOcamlTensorType env =
  | TyFloat _ -> CTyDouble ()
  | TyInt _ -> CTyInt64 ()
  | ty -> errorSingle [infoTy ty] "Type is not supported for CUDA tensors"

  sem _generateCDataRepresentation : CWrapperEnv -> Type -> CDataRepr
  sem _generateCDataRepresentation env =
  | ty & (TySeq t) ->
    match env.targetEnv with CudaTargetEnv cenv in
    let elemTy = _unwrapType cenv.compileCEnv.typeEnv t.ty in
    CudaSeqRepr {
      ident = nameSym "cuda_seq_tmp",
      data = _generateCDataRepresentation env elemTy,
      elemTy = getCudaType env.targetEnv t.ty, ty = getCudaType env.targetEnv ty}
  | ty & (TyTensor t) ->
    CudaTensorRepr {
      ident = nameSym "cuda_tensor_tmp",
      data = _generateCDataRepresentation env t.ty,
      elemTy = getCudaType env.targetEnv t.ty,
      ty = getCudaType env.targetEnv ty}
  | ty & (TyRecord t) ->
    match env.targetEnv with CudaTargetEnv cenv in
    let labels = tyRecordOrderedLabels ty in
    let fields : [CDataRepr] =
      map
        (lam label : SID.
          match mapLookup label t.fields with Some ty then
            let ty = _unwrapType cenv.compileCEnv.typeEnv ty in
            _generateCDataRepresentation env ty
          else errorSingle [t.info] "Inconsistent labels in record type")
        labels in
    CudaRecordRepr {
      ident = nameSym "cuda_rec_tmp", labels = labels, fields = fields,
      ty = getCudaType env.targetEnv ty}
  | ty & (TyVariant t) ->
    match env.targetEnv with CudaTargetEnv cenv in
    let constrs : Map Name (CType, CDataRepr) =
      mapMapWithKey
        (lam constrId : Name. lam constrTy : Type.
          let constrTy = _unwrapType cenv.compileCEnv.typeEnv constrTy in
          ( getCudaType env.targetEnv constrTy
          , _generateCDataRepresentation env constrTy ))
        t.constrs in
    CudaDataTypeRepr {
      ident = nameSym "cuda_adt_tmp", constrs = constrs,
      ty = getCudaType env.targetEnv ty}
  | ty & (TyCon _) ->
    match env.targetEnv with CudaTargetEnv cenv in
    let ty = _unwrapType cenv.compileCEnv.typeEnv ty in
    match ty with TyCon _ then errorSingle [infoTy ty] "Could not unwrap type"
    else _generateCDataRepresentation env ty
  | TyAlias t ->
    _generateCDataRepresentation env t.content
  | ty ->
    CudaBaseTypeRepr {
      ident = nameSym "cuda_tmp",
      ty = getCudaType env.targetEnv ty}

  -- Applies an operation on all tensors contained in an expression of a given
  -- data representation.
  sem mapTensorsToStmts : CWrapperEnv
                       -> (CWrapperEnv -> CExpr -> CExpr -> [CStmt])
                       -> CExpr -> CDataRepr -> [CStmt]
  sem mapTensorsToStmts env tensorFn src =
  | CudaSeqRepr t ->
    let iterId = nameSym "i" in
    let iter = CEVar {id = iterId} in
    let innerSrc = CEApp {
      fun = _getIdentExn "Field",
      args = [src, iter]} in
    let stmts = mapTensorsToStmts env tensorFn innerSrc t.data in
    if null stmts then []
    else
      let iterInitStmt = CSDef {
        ty = CTyInt64 (), id = Some iterId,
        init = Some (CIExpr {expr = CEInt {i = 0}})} in
      let iterIncrementStmt = CSExpr {expr = CEBinOp {
        op = COAssign (),
        lhs = iter,
        rhs = CEBinOp {op = COAdd (), lhs = iter, rhs = CEInt {i = 1}}}} in
      let lenExpr = CEApp {fun = _getIdentExn "Wosize_val", args = [src]} in
      let loopStmt = CSWhile {
        cond = CEBinOp {op = COLt (), lhs = iter, rhs = lenExpr},
        body = snoc stmts iterIncrementStmt} in
      [iterInitStmt, loopStmt]
  | CudaRecordRepr t ->
    foldl
      (lam acc. lam field : (SID, (Int, CDataRepr)).
        match field with (key, (idx, fieldRepr)) in
        let fieldId = nameNoSym (sidToString key) in
        let innerSrc = CEApp {
          fun = _getIdentExn "Field",
          args = [src, CEInt {i = idx}]} in
        let stmts = mapTensorsToStmts env tensorFn innerSrc fieldRepr in
        concat acc stmts)
      [] (zip t.labels (create (length t.fields) (lam i. (i, get t.fields i))))
  | CudaDataTypeRepr t ->
    let counter = ref 0 in
    mapFoldWithKey
      (lam acc. lam constrId. lam constrTyData.
        match constrTyData with (constrTy, constrData) in
        let constrExpr = _accessMember t.ty src constrId in
        let count = deref counter in
        modref counter (addi count 1);
        let stmts = mapTensorsToStmts env tensorFn constrExpr constrData in
        if null stmts then acc
        else
          [ CSIf {
              cond = CEBinOp {
                op = COEq (),
                lhs = _accessMember t.ty src _constrKey,
                rhs = CEInt {i = count}},
              thn = stmts,
              els = acc} ])
      [] t.constrs
  | CudaBaseTypeRepr _ -> []
  | CudaTensorRepr t ->
    let elemTypeId =
      match t.elemTy with CTyInt64 _ | CTyDouble _ then 0
      else match t.elemTy with CTyInt32 _ then 1
      else match t.elemTy with CTyFloat _ then 2
      else never in
    let elemType = CEInt {i = elemTypeId} in
    tensorFn env elemType src
end

lang OCamlToCudaWrapper = CudaCWrapperBase
  sem _generateOCamlToCudaWrapperStmts : CWrapperEnv -> CExpr -> Name
                                      -> CDataRepr -> [CStmt]
  sem _generateOCamlToCudaWrapperStmts env src dstIdent =
  | CudaSeqRepr t ->
    let seqInitExpr =
      match t.ty with CTyPtr {ty = ty} then
        let sizeExpr = CESizeOfType {ty = ty} in
        Some (CECast {ty = t.ty, rhs = CEApp {fun = _malloc, args = [sizeExpr]}})
      else None () in
    let seqDefStmt = CSDef {
      ty = t.ty, id = Some dstIdent,
      init = optionMap (lam e. CIExpr {expr = e}) seqInitExpr} in
    let dst = CEVar {id = dstIdent} in
    let seqLenExpr = _accessMember t.ty dst _seqLenKey in
    let setLenStmt = CSExpr {expr = CEBinOp {
      op = COAssign (), lhs = seqLenExpr, rhs = _wosize src}} in
    let elemTy = t.elemTy in
    let sizeExpr = CEBinOp {
      op = COMul (),
      lhs = _wosize src,
      rhs = CESizeOfType {ty = elemTy}} in
    let allocSeqStmt = CSExpr {expr = CEApp {
      fun = _cudaMallocManaged,
      args = [
        CEUnOp {op = COAddrOf (), arg = _accessMember t.ty dst _seqKey},
        sizeExpr]}} in

    let iterIdent = nameSym "i" in
    let iter = CEVar {id = iterIdent} in
    let iterDefStmt = CSDef {
      ty = CTyInt64 (), id = Some iterIdent,
      init = Some (CIExpr {expr = CEInt {i = 0}})} in
    let fieldDstExpr = CEBinOp {
      op = COSubScript (),
      lhs = _accessMember t.ty dst _seqKey,
      rhs = iter} in
    let fieldUpdateStmts =
      match elemTy with CTyFloat _ | CTyDouble _ then
        [ CSExpr {expr = CEBinOp {
            op = COAssign (),
            lhs = fieldDstExpr,
            rhs = CECast {
              ty = elemTy,
              rhs = CEApp {
                fun = _getIdentExn "Double_field",
                args = [src, iter]}}}} ]
      else
        let fieldId = nameSym "cuda_seq_temp" in
        let fieldExpr = CEApp {fun = _getIdentExn "Field", args = [src, iter]} in
        let fieldDefStmt = CSDef {
          ty = elemTy, id = Some fieldId,
          init = Some (CIExpr {expr = fieldExpr})} in
        let stmts = _generateOCamlToCudaWrapperStmts env fieldExpr fieldId t.data in
        let fieldUpdateStmt = CSExpr {expr = CEBinOp {
          op = COAssign (),
          lhs = fieldDstExpr,
          rhs = CEVar {id = fieldId}}} in
        snoc stmts fieldUpdateStmt in
    let iterIncrementStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = iter,
      rhs = CEBinOp {op = COAdd (), lhs = iter, rhs = CEInt {i = 1}}}} in
    let dataCopyLoopStmt = CSWhile {
      cond = CEBinOp {op = COLt (), lhs = iter, rhs = seqLenExpr},
      body = snoc fieldUpdateStmts iterIncrementStmt} in
    [ seqDefStmt, setLenStmt, allocSeqStmt, _cudaErrorCheckStmt, iterDefStmt, dataCopyLoopStmt ]
  | CudaTensorRepr t ->
    match env.targetEnv with CudaTargetEnv cenv in
    let dst = CEVar {id = dstIdent} in
    let tensorDefStmt = CSDef {ty = t.ty, id = Some dstIdent, init = None ()} in
    let getField = lam v. lam idx.
      CEApp {fun = _getIdentExn "Field", args = [v, idx]}
    in
    let longVal = lam arg. CEApp {fun = _getIdentExn "Long_val", args = [arg]} in
    -- NOTE(larshum, 2023-11-03): I don't understand why, but all tensors seem
    -- to have a layer of indirection which we bypass here.
    let tensorVal = getField src (CEInt {i = 0}) in
    let setTensorDataStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = _accessMember t.ty dst _tensorDataId,
      rhs = CEBinOp {
        op = COAdd (),
        lhs = CECast {
          ty = CTyPtr {ty = t.elemTy},
          rhs = CEApp {
            fun = _getIdentExn "Data_custom_val",
            args = [getField tensorVal (CEInt {i = 0})]}},
        rhs = longVal (getField tensorVal (CEInt {i = 3}))}}} in
    let setTensorRankStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = _accessMember t.ty dst _tensorRankId,
      rhs = longVal (getField tensorVal (CEInt {i = 2}))}} in
    let setTensorSizeStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = _accessMember t.ty dst _tensorSizeId,
      rhs = longVal (getField tensorVal (CEInt {i = 4}))}} in

    let iterIdent = nameSym "i" in
    let iter = CEVar {id = iterIdent} in
    let setTensorDimStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = CEBinOp {
        op = COSubScript (),
        lhs = _accessMember t.ty dst _tensorDimsId,
        rhs = iter},
      rhs = longVal (getField (getField tensorVal (CEInt {i = 1})) iter)}} in
    let iterDefStmt = CSDef {
      ty = CTyInt64 (), id = Some iterIdent,
      init = Some (CIExpr {expr = CEInt {i = 0}})} in
    let iterIncrementStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = iter,
      rhs = CEBinOp {
        op = COAdd (), lhs = iter, rhs = CEInt {i = 1}}}} in
    let setTensorDimsStmt = CSWhile {
      cond = CEBinOp {op = COLt (), lhs = iter,
                      rhs = _accessMember t.ty dst _tensorRankId},
      body = [setTensorDimStmt, iterIncrementStmt]} in
    [ tensorDefStmt, setTensorDataStmt, setTensorRankStmt, iterDefStmt
    , setTensorDimsStmt, setTensorSizeStmt ]
  | CudaRecordRepr t ->
    let dst = CEVar {id = dstIdent} in
    let generateMarshallingField : [CStmt] -> (CDataRepr, Int) -> [CStmt] =
      lam acc. lam fieldIdx.
      match fieldIdx with (field, idx) in
      let valueType = CTyVar {id = _getIdentExn "value"} in
      let srcExpr = CEApp {
        fun = _getIdentExn "Field",
        args = [src, CEInt {i = idx}]} in
      let labelId : Name = nameNoSym (sidToString (get t.labels idx)) in
      let fieldDstIdent = nameSym "cuda_rec_field" in
      let innerStmts = _generateOCamlToCudaWrapperStmts env srcExpr fieldDstIdent field in
      let fieldUpdateStmt = CSExpr {expr = CEBinOp {
        op = COAssign (),
        lhs = _accessMember t.ty dst labelId,
        rhs = CEVar {id = fieldDstIdent}}} in
      join [acc, innerStmts, [fieldUpdateStmt]]
    in
    let indexedFields = create (length t.fields) (lam i. (get t.fields i, i)) in
    let recordInitExpr =
      match t.ty with CTyPtr {ty = ty} then
        let sizeExpr = CESizeOfType {ty = ty} in
        Some (CECast {ty = t.ty, rhs = CEApp {fun = _malloc, args = [sizeExpr]}})
      else None () in
    let recordDefStmt = CSDef {
      ty = t.ty, id = Some dstIdent,
      init = optionMap (lam e. CIExpr {expr = e}) recordInitExpr} in
    cons
      recordDefStmt
      (foldl generateMarshallingField [] indexedFields)
  | CudaDataTypeRepr t ->
    let sizeExpr = CESizeOfType {ty = t.ty} in
    let tagValExpr = CEApp {fun = nameNoSym "Tag_val", args = [src]} in
    let unknownTagStmt = CSExpr {expr = CEApp {
      fun = _printf,
      args = [
        CEString {s = "Unknown constructor with tag %d\n"},
        tagValExpr]}} in
    let dst = CEVar {id = dstIdent} in
    let dstAllocStmt = CSDef {
      ty = t.ty, id = Some dstIdent,
      init = Some (CIExpr {expr = CECast {
        ty = t.ty,
        rhs = CEApp {
          fun = _malloc,
          args = [CESizeOfType {ty = _stripPointer t.ty}]}}})} in
    let dst = CEVar {id = dstIdent} in
    -- NOTE(larshum, 2022-03-29): Use a counter to keep track of which
    -- constructor we are currently at.
    let counter = ref 0 in
    let constructorInitStmt =
      mapFoldWithKey
        (lam acc : CStmt. lam constrId : Name. lam v : (CType, CDataRepr).
          match v with (constrTy, constrData) in
          let setConstrStmt = CSExpr {expr = CEBinOp {
            op = COAssign (),
            lhs = CEArrow {lhs = dst, id = _constrKey},
            rhs = CEVar {id = constrId}}} in
          let innerId = nameSym "cuda_constr_inner_tmp" in
          let getFieldFirst : CExpr -> CExpr = lam expr.
            CEApp {fun = _getIdentExn "Field", args = [expr, CEInt {i = 0}]} in
          let srcExpr =
            match constrData with CudaRecordRepr _ then src
            else getFieldFirst src in
          let setTempStmts =
            _generateOCamlToCudaWrapperStmts env srcExpr innerId constrData in
          let setValueStmt = CSExpr {expr = CEBinOp {
            op = COAssign (),
            lhs = CEArrow {lhs = dst, id = constrId},
            rhs = CEVar {id = innerId}}} in
          let body = join [[setConstrStmt], setTempStmts, [setValueStmt]] in
          let count = deref counter in
          (modref counter (addi count 1));
          CSIf {
            cond = CEBinOp {
              op = COEq (),
              lhs = tagValExpr,
              rhs = CEInt {i = count}},
            thn = body, els = [acc]})
        unknownTagStmt t.constrs in
    [dstAllocStmt, constructorInitStmt]
  | CudaBaseTypeRepr t ->
    [ CSDef {ty = t.ty, id = Some dstIdent, init = Some (CIExpr {expr = CEApp {
        fun = ocamlToCConversionFunctionIdent t.ty,
        args = [src]}})} ]

  sem _generateOCamlToCudaWrapperArg : CWrapperEnv -> [CStmt] -> ArgData -> [CStmt]
  sem _generateOCamlToCudaWrapperArg env acc =
  | {mexprIdent = mid, copyStatus = cs, gpuIdent = gid, cData = cdata} ->
    let src = CEVar {id = mid} in
    concat acc (_generateOCamlToCudaWrapperStmts env src gid cdata)

  sem generateOCamlToCudaWrapper : CWrapperEnv -> [CStmt]
  sem generateOCamlToCudaWrapper =
  | env ->
    foldl (_generateOCamlToCudaWrapperArg env) [] env.arguments
end

lang CudaCallWrapper = CudaCWrapperBase
  sem generateCudaWrapperCall : CWrapperEnv -> [CStmt]
  sem generateCudaWrapperCall =
  | env ->
    match env.targetEnv with CudaTargetEnv cenv in
    let return : ArgData = env.return in
    let returnType = return.mexprType in
    let cudaType = getCudaType env.targetEnv returnType in
    let args : [CExpr] =
      map
        (lam arg : ArgData. CEVar {id = arg.gpuIdent})
        env.arguments in
    let cudaWrapperId =
      match mapLookup env.functionIdent cenv.wrapperMap with Some id then id
      else error "Internal compiler error: No function defined for wrapper map" in
    match return.cData with CudaRecordRepr {fields = []} then
      let wrapperCallStmt = CSExpr {expr = CEApp {
        fun = cudaWrapperId, args = args}} in
      [wrapperCallStmt]
    else
      let returnDecl = CSDef {
        ty = cudaType, id = Some return.gpuIdent, init = None ()} in
      let cudaWrapperCallStmt = CSExpr {expr = CEBinOp {
        op = COAssign (),
        lhs = CEVar {id = return.gpuIdent},
        rhs = CEApp {fun = cudaWrapperId, args = args}}} in
      [returnDecl, cudaWrapperCallStmt]
end

lang CudaToOCamlWrapper = CudaCWrapperBase
  sem _generateCudaToOCamlWrapperH : CWrapperEnv -> CExpr -> CExpr -> CDataRepr
                                  -> [CStmt]
  sem _generateCudaToOCamlWrapperH env src dst =
  | CudaSeqRepr t ->
    let lengthExpr = _accessMember t.ty src _seqLenKey in
    let tagExpr =
      match t.elemTy with CTyFloat _ | CTyDouble _ then
        CEVar {id = _getIdentExn "Double_array_tag"}
      else CEInt {i = 0} in
    let seqAllocStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = src,
      rhs = CEApp {
        fun = _getIdentExn "caml_alloc", args = [lengthExpr, tagExpr]}}} in
    let iterId = nameSym "i" in
    let iter = CEVar {id = iterId} in
    let iterInitStmt = CSDef {
      ty = CTyInt64 (), id = Some iterId,
      init = Some (CIExpr {expr = CEInt {i = 0}})} in
    let srcExpr = CEBinOp {op = COSubScript (), lhs = dst, rhs = iter} in
    let fieldStoreStmts =
      match t.elemTy with CTyFloat _ | CTyDouble _ then
        [ CSExpr {expr = CEApp {
            fun = _getIdentExn "Store_double_field",
            args = [dst, iter, srcExpr]}} ]
      else
        let fieldId = nameSym "cuda_seq_temp" in
        let field = CEVar {id = fieldId} in
        let fieldDefStmt = CSDef {
          ty = t.elemTy, id = Some fieldId, init = None ()} in
        let innerStmts = _generateCudaToOCamlWrapperH env srcExpr field t.data in
        let storeFieldStmt = CSExpr {expr = CEApp {
          fun = _getIdentExn "Store_field",
          args = [dst, iter, CEVar {id = fieldId}]}} in
        join [[fieldDefStmt], innerStmts, [storeFieldStmt]] in
    let iterIncrementStmt = CSExpr {expr = CEBinOp {
      op = COAssign (),
      lhs = iter,
      rhs = CEBinOp {op = COAdd (), lhs = iter, rhs = CEInt {i = 1}}}} in
    let storeLoopStmt = CSWhile {
      cond = CEBinOp {op = COLt (), lhs = iter, rhs = lengthExpr},
      body = snoc fieldStoreStmts iterIncrementStmt} in
    [seqAllocStmt, iterInitStmt, storeLoopStmt]
  | CudaTensorRepr t ->
    error "Tensors cannot be returned from accelerated code"
  | CudaRecordRepr t ->
    if null t.fields then []
    else
      let wrapField = lam idx : Int. lam field : CDataRepr.
        let labelId = nameNoSym (sidToString (get t.labels idx)) in
        let fieldSrcExpr = _accessMember t.ty src labelId in
        let tempId = nameSym "cuda_rec_tmp" in
        let temp = CEVar {id = tempId} in
        let fieldStmts = _generateCudaToOCamlWrapperH env fieldSrcExpr temp field in
        let storeStmt = CSExpr {expr = CEApp {
          fun = _getIdentExn "Store_field",
          args = [dst, CEInt {i = idx}, temp]}} in
        snoc fieldStmts storeStmt
      in
      let recordAllocStmt = CSExpr {expr = CEBinOp {
        op = COAssign (),
        lhs = dst,
        rhs = CEApp {
          fun = _getIdentExn "caml_alloc",
          args = [CEInt {i = length t.fields}, CEInt {i = 0}]}}} in
      let fieldStmts = join (mapi wrapField t.fields) in
      cons recordAllocStmt fieldStmts
  | CudaDataTypeRepr t ->
    error "Data types cannot be returned from accelerated code"
  | CudaBaseTypeRepr t ->
    [ CSExpr {
        expr = CEBinOp {
          op = COAssign (),
          lhs = dst,
          rhs = CEApp {
            fun = cToOCamlConversionFunctionIdent t.ty,
            args = [CECast {ty = t.ty, rhs = src}]}}} ]

  sem generateCudaToOCamlWrapper =
  | env ->
    let env : CWrapperEnv = env in
    let return = env.return in
    let src = CEVar {id = return.gpuIdent} in
    let dst = CEVar {id = return.mexprIdent} in
    _generateCudaToOCamlWrapperH env src dst return.cData
end

lang CudaCWrapper =
  OCamlToCudaWrapper + CudaCallWrapper + CudaToOCamlWrapper + Cmp

  -- Generates the initial wrapper environment
  sem generateInitWrapperEnv : Map Name Name -> CompileCEnv -> Int -> CWrapperEnv
  sem generateInitWrapperEnv wrapperMap compileCEnv =
  | tensorMaxRank ->
    let compileCEnv : CompileCEnv = compileCEnv in
    let tupleSwap = lam t : (Name, Type). match t with (x, y) in (y, x) in
    let revTypeEnv = mapFromSeq cmpType (map tupleSwap compileCEnv.typeEnv) in
    let targetEnv = CudaTargetEnv {
      wrapperMap = wrapperMap, compileCEnv = compileCEnv,
      revTypeEnv = revTypeEnv, tensorMaxRank = tensorMaxRank} in
    let env : CWrapperEnv = _emptyWrapperEnv () in
    {env with targetEnv = targetEnv}

  sem generateMarshallingCode =
  | env ->
    let stmt5 = generateOCamlToCudaWrapper env in
    let stmt6 = generateCudaWrapperCall env in
    let stmt7 = generateCudaToOCamlWrapper env in
    join [stmt5, stmt6, stmt7]

  -- Defines the target-specific generation of wrapper code.
  sem generateWrapperCode : Map Name AccelerateData -> Map Name Name
                         -> Int -> CompileCEnv -> CudaProg
  sem generateWrapperCode accelerated wrapperMap tensorMaxRank =
  | compileCEnv ->
    let env = generateInitWrapperEnv wrapperMap compileCEnv tensorMaxRank in
    match generateWrapperCodeH env accelerated with (env, entryPointWrappers) in
    let entryPointTops =
      map
        (lam top : CTop. CuTTop {attrs = [CuAExternC ()], top = top})
        entryPointWrappers in
    CuPProg {
      includes = [
        "<stddef.h>",
        "<stdlib.h>",
        "<stdio.h>",
        "\"caml/alloc.h\"",
        "\"caml/bigarray.h\"",
        "\"caml/memory.h\"",
        "\"caml/mlvalues.h\"",
        "\"cuda-utils.cuh\""
      ],
      tops = entryPointTops}
end
