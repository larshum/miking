-- Eliminates occurrences of polymorphic types in the provided (typed) MExpr
-- AST, by replacing polymorphic functions by multiple monomorphic functions
-- (one for each distinct combination of types used to invoke the function).

include "mexpr/ast.mc"
include "mexpr/cmp.mc"
include "mexpr/eq.mc"
include "mexpr/eval.mc"
include "mexpr/pprint.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"

lang Monomorphize = MExprAst + MExprCmp + MExprPrettyPrint
  -- An instantiation maps type variable identifiers to concrete types. It
  -- represents a monomorphic use of a polymorphic construct.
  type Instantiation = Map Name Type

  sem emptyInstantiation : () -> Instantiation
  sem emptyInstantiation =
  | _ -> mapEmpty nameCmp

  sem cmpInstantiation : Instantiation -> Instantiation -> Int
  sem cmpInstantiation lhs =
  | rhs -> mapCmp cmpType lhs rhs

  type InstEntry = {
    map : Map Instantiation Name,
    polyType : Type
  }

  sem defaultInstEntry : Type -> InstEntry
  sem defaultInstEntry =
  | ty -> {map = mapEmpty cmpInstantiation, polyType = ty}

  type MonoEnv = {
    funEnv : Map Name InstEntry,
    conEnv : Map Name InstEntry,
    typeEnv : Map Name InstEntry,
    constEnv : Map Const InstEntry
  }

  sem emptyMonoEnv : () -> MonoEnv
  sem emptyMonoEnv =
  | _ ->
    { funEnv = mapEmpty nameCmp, conEnv = mapEmpty nameCmp
    , typeEnv = mapEmpty nameCmp, constEnv = mapEmpty cmpConst }

  -- NOTE(larshum, 2023-08-07): Provides a readable overview of the contents of
  -- the monomorphization environment, for debugging purposes.
  sem monoEnvToString : MonoEnv -> String
  sem monoEnvToString =
  | env ->
    let innerEnvToString = lam innerEnv. lam printId.
      mapFoldWithKey
        (lam acc. lam id. lam entry.
          let acc = snoc acc (printId id) in
          let acc = snoc acc (type2str entry.polyType) in
          mapFoldWithKey
            (lam acc. lam inst. lam.
              mapFoldWithKey
                (lam acc. lam varId. lam ty.
                  snoc acc (join ["  ", nameGetStr varId, " -> ", type2str ty]))
                acc inst)
            acc entry.map)
        [] innerEnv
    in
    strJoin "\n" [
      "Functions",
      strJoin "\n" (innerEnvToString env.funEnv nameGetStr),
      "Constructors",
      strJoin "\n" (innerEnvToString env.conEnv nameGetStr),
      "Types",
      strJoin "\n" (innerEnvToString env.typeEnv nameGetStr),
      "Constants",
      strJoin "\n" (innerEnvToString env.constEnv (getConstStringCode 0)) ]

  -- Collects the type parameters
  sem collectTypeAppParams : [Type] -> Type -> (Type, [Type])
  sem collectTypeAppParams acc =
  | TyApp t ->
    collectTypeAppParams (cons t.rhs acc) t.lhs
  | ty ->
    (ty, acc)
end

lang MonomorphizeInstantiate = Monomorphize
  -- Given a polymorphic type and a corresponding monomorphic type, we find the
  -- type instantiation this corresponds to.
  sem findTypeInstantiation : Type -> Type -> Instantiation
  sem findTypeInstantiation polyType =
  | monoType ->
    let polyType = inspectType polyType in
    let inst = inferInstantiation (emptyInstantiation ()) (polyType, monoType) in
    -- NOTE(larshum, 2023-08-03): If any resulting type contains a TyUnknown,
    -- we replace it with the unit type to make it concrete.
    mapMapWithKey (lam. lam ty. replaceUnknownWithUnit ty) inst

  -- Given the type parameter identifiers and the monomorphic types they are
  -- mapped to, we construct an instantiation.
  sem constructTypeInstantiation : [Name] -> [Type] -> Instantiation
  sem constructTypeInstantiation paramIds =
  | paramTypes ->
    let inst = mapFromSeq nameCmp (zip paramIds paramTypes) in
    mapMapWithKey (lam. lam ty. replaceUnknownWithUnit ty) inst

  sem replaceUnknownWithUnit : Type -> Type
  sem replaceUnknownWithUnit =
  | TyUnknown {info = info} -> TyRecord {fields = mapEmpty cmpSID, info = info}
  | ty -> smap_Type_Type replaceUnknownWithUnit ty

  -- Infers the instantiation of type variables represented by a monomorphic
  -- types and its corresponding polymorphic type.
  sem inferInstantiation : Instantiation -> (Type, Type) -> Instantiation
  sem inferInstantiation inst =
  | (TyVar {ident = ident}, monoType) ->
    mapInsert ident monoType inst
  | (TyArrow l, TyArrow r) ->
    let inst = inferInstantiation inst (l.from, r.from) in
    inferInstantiation inst (l.to, r.to)
  | (TySeq l, TySeq r) ->
    inferInstantiation inst (l.ty, r.ty)
  | (TyTensor l, TyTensor r) ->
    inferInstantiation inst (l.ty, r.ty)
  | (TyRecord l, TyRecord r) ->
    let mergefn = lam lhs. lam rhs.
      match (lhs, rhs) with (Some lty, Some rty) then
        Some (lty, rty)
      else
        error "Monomorphization error: Record type field mismatch"
    in
    let f = lam inst. lam. lam tyPair.
      inferInstantiation inst tyPair
    in
    mapFoldWithKey f inst (mapMerge mergefn l.fields r.fields)
  | (TyApp l, TyApp r) ->
    let inst = inferInstantiation inst (l.lhs, r.lhs) in
    inferInstantiation inst (l.rhs, r.rhs)
  | (TyAlias l, TyAlias r) ->
    inferInstantiation inst (l.display, r.display)
  | (lty, rty) ->
    -- NOTE(larshum, 2023-08-03): In other cases, we accept the type and ignore
    -- its contents if both types have the same constructor tag.
    if eqi (constructorTag lty) (constructorTag rty) then inst
    else
      error "Monomorphization error: Unsupported polymorphic type instantiation"

  -- Applies an instantiation on a provided expression, producing a
  -- monomorphized expression over those variables.
  sem instantiatePolymorphicExpr : Instantiation -> Expr -> Expr
  sem instantiatePolymorphicExpr inst =
  | t ->
    let t = smap_Expr_Expr (instantiatePolymorphicExpr inst) t in
    let t = smap_Expr_Type (instantiatePolymorphicType inst) t in
    let t = smap_Expr_TypeLabel (instantiatePolymorphicType inst) t in
    let t = smap_Expr_Pat (instantiatePolymorphicPat inst) t in
    withType (instantiatePolymorphicType inst (tyTm t)) t

  sem instantiatePolymorphicPat : Instantiation -> Pat -> Pat
  sem instantiatePolymorphicPat inst =
  | p ->
    let p = smap_Pat_Pat (instantiatePolymorphicPat inst) p in
    withTypePat (instantiatePolymorphicType inst (tyPat p)) p

  -- Applies an instantiation to a provided polymorphic type to produce a
  -- monomorphized type with respect to the instantiated variables.
  sem instantiatePolymorphicType : Instantiation -> Type -> Type
  sem instantiatePolymorphicType inst =
  | TyVar t ->
    match mapLookup t.ident inst with Some ty then ty
    else TyVar t
  | TyAll t -> instantiatePolymorphicType inst t.ty
  | ty -> smap_Type_Type (instantiatePolymorphicType inst) ty
end

lang MonomorphizeResymbolize = Monomorphize
  -- Resymbolizes all variables bound inside the provided expression. We use
  -- this to ensure function definitions duplicated due to monomorphization end
  -- up with distinct symbols.
  sem resymbolizeBindings : Expr -> Expr
  sem resymbolizeBindings =
  | ast -> resymbolizeBindingsExpr (mapEmpty nameCmp) ast

  sem resymbolizeBindingsExpr : Map Name Name -> Expr -> Expr
  sem resymbolizeBindingsExpr nameMap =
  | TmVar t ->
    let newId =
      match mapLookup t.ident nameMap with Some newId then newId
      else t.ident
    in
    TmVar {t with ident = newId, ty = resymbolizeBindingsType nameMap t.ty}
  | TmLam t ->
    let newId = nameSetNewSym t.ident in
    let nameMap = mapInsert t.ident newId nameMap in
    TmLam {t with ident = newId,
                  tyAnnot = resymbolizeBindingsType nameMap t.tyAnnot,
                  tyParam = resymbolizeBindingsType nameMap t.tyParam,
                  body = resymbolizeBindingsExpr nameMap t.body,
                  ty = resymbolizeBindingsType nameMap t.ty}
  | TmLet t ->
    let body = resymbolizeBindingsExpr nameMap t.body in
    let newId = nameSetNewSym t.ident in
    let nameMap = mapInsert t.ident newId nameMap in
    TmLet {t with ident = newId,
                  tyAnnot = resymbolizeBindingsType nameMap t.tyAnnot,
                  tyBody = resymbolizeBindingsType nameMap t.tyBody,
                  body = body,
                  inexpr = resymbolizeBindingsExpr nameMap t.inexpr,
                  ty = resymbolizeBindingsType nameMap t.ty}
  | TmRecLets t ->
    let addNewIdBinding = lam nameMap. lam bind.
      let newId = nameSetNewSym bind.ident in
      (mapInsert bind.ident newId nameMap, {bind with ident = newId})
    in
    match mapAccumL addNewIdBinding nameMap t.bindings with (nameMap, bindings) in
    let resymbolizeBind = lam bind.
      {bind with tyAnnot = resymbolizeBindingsType nameMap bind.tyAnnot,
                 tyBody = resymbolizeBindingsType nameMap bind.tyBody,
                 body = resymbolizeBindingsExpr nameMap bind.body}
    in
    let bindings = map resymbolizeBind bindings in
    TmRecLets {t with bindings = bindings,
                      inexpr = resymbolizeBindingsExpr nameMap t.inexpr,
                      ty = resymbolizeBindingsType nameMap t.ty}
  | TmType t ->
    let newId = nameSetNewSym t.ident in
    let nameMap = mapInsert t.ident newId nameMap in
    TmType {t with ident = newId,
                   tyIdent = resymbolizeBindingsType nameMap t.tyIdent,
                   inexpr = resymbolizeBindingsExpr nameMap t.inexpr,
                   ty = resymbolizeBindingsType nameMap t.ty}
  | TmConDef t ->
    let newId = nameSetNewSym t.ident in
    let nameMap = mapInsert t.ident newId nameMap in
    TmConDef {t with ident = newId,
                     inexpr = resymbolizeBindingsExpr nameMap t.inexpr,
                     ty = resymbolizeBindingsType nameMap t.ty}
  | TmConApp t ->
    let newId =
      match mapLookup t.ident nameMap with Some newId then newId
      else t.ident
    in
    TmConApp {t with ident = newId,
                     body = resymbolizeBindingsExpr nameMap t.body,
                     ty = resymbolizeBindingsType nameMap t.ty}
  | TmMatch t ->
    let target = resymbolizeBindingsExpr nameMap t.target in
    match resymbolizeBindingsPat nameMap t.pat with (thnNameMap, pat) in
    TmMatch {t with target = target, pat = pat,
                    thn = resymbolizeBindingsExpr thnNameMap t.thn,
                    els = resymbolizeBindingsExpr nameMap t.els,
                    ty = resymbolizeBindingsType nameMap t.ty}
  | t ->
    let t = smap_Expr_Expr (resymbolizeBindingsExpr nameMap) t in
    let t = smap_Expr_Type (resymbolizeBindingsType nameMap) t in
    let t = smap_Expr_TypeLabel (resymbolizeBindingsType nameMap) t in
    withType (resymbolizeBindingsType nameMap (tyTm t)) t

  sem resymbolizeBindingsPat : Map Name Name -> Pat -> (Map Name Name, Pat)
  sem resymbolizeBindingsPat nameMap =
  | PatNamed (t & {ident = PName id}) ->
    let newId = nameSetNewSym id in
    (mapInsert id newId nameMap, PatNamed {t with ident = PName newId})
  | PatSeqEdge (t & {middle = PName id}) ->
    let newId = nameSetNewSym id in
    (mapInsert id newId nameMap, PatSeqEdge {t with middle = PName newId})
  | PatCon t ->
    match mapLookup t.ident nameMap with Some newId then
      (nameMap, PatCon {t with ident = newId})
    else (nameMap, PatCon t)
  | p -> smapAccumL_Pat_Pat resymbolizeBindingsPat nameMap p

  sem resymbolizeBindingsType : Map Name Name -> Type -> Type
  sem resymbolizeBindingsType nameMap =
  | TyCon t ->
    match mapLookup t.ident nameMap with Some newId then
      TyCon {t with ident = newId}
    else TyCon t
  | TyVar t ->
    match mapLookup t.ident nameMap with Some newId then
      TyVar {t with ident = newId}
    else TyVar t
  | TyAll t ->
    let newId = nameSetNewSym t.ident in
    let nameMap = mapInsert t.ident newId nameMap in
    TyAll {t with ident = newId,
                  ty = resymbolizeBindingsType nameMap t.ty}
  | ty -> smap_Type_Type (resymbolizeBindingsType nameMap) ty
end

lang MonomorphizeCollect = MonomorphizeInstantiate
  -- Collects the monomorphic instantiations of polymorphic constructs of the
  -- provided AST. This is performed in two passes:
  --
  -- 1. Record the definitions of polymorphic constructs (top-down)
  -- 2. Collect monomorphic instantiations of the constructs (bottom-up)
  --
  -- The resulting environment defines how to monomorphize the provided AST.
  sem collectInstantiations : Expr -> MonoEnv
  sem collectInstantiations =
  | ast ->
    let env = recordPolymorphicDefinitions (emptyMonoEnv ()) ast in
    collectInstantiationsExpr [emptyInstantiation ()] env ast

  sem recordPolymorphicDefinitions : MonoEnv -> Expr -> MonoEnv
  sem recordPolymorphicDefinitions env =
  | TmLet t ->
    let env =
      match t.tyBody with TyAll _ then
        {env with funEnv = mapInsert t.ident (defaultInstEntry t.tyBody) env.funEnv}
      else env
    in
    let env = recordPolymorphicDefinitions env t.body in
    recordPolymorphicDefinitions env t.inexpr
  | TmRecLets t ->
    let recordBind = lam env. lam bind.
      let env =
        match bind.tyBody with TyAll _ then
          {env with funEnv = mapInsert bind.ident (defaultInstEntry bind.tyBody) env.funEnv}
        else env
      in
      recordPolymorphicDefinitions env bind.body
    in
    let env = foldl recordBind env t.bindings in
    recordPolymorphicDefinitions env t.inexpr
  | TmType t ->
    let env =
      if not (null t.params) then
        -- NOTE(larshum, 2023-08-03): We construct a polymorphic type
        -- representation of the type definition so that we can treat them in
        -- the same way as other constructs later.
        let polyType =
          foldr
            ntyall_
            (foldl
              (lam accTy. lam paramId. tyapp_ accTy (ntyvar_ paramId))
              (ntycon_ t.ident) t.params)
            t.params
        in
        {env with typeEnv = mapInsert t.ident (defaultInstEntry polyType) env.typeEnv}
      else env
    in
    recordPolymorphicDefinitions env t.inexpr
  | TmConDef t ->
    let env =
      match t.tyIdent with TyAll _ then
        {env with conEnv = mapInsert t.ident (defaultInstEntry t.tyIdent) env.conEnv}
      else env
    in
    recordPolymorphicDefinitions env t.inexpr
  | ast -> sfold_Expr_Expr recordPolymorphicDefinitions env ast

  sem collectInstantiationsExpr : [Instantiation] -> MonoEnv -> Expr -> MonoEnv
  sem collectInstantiationsExpr instantiations env =
  | TmVar t ->
    let env = collectInstantiationsType instantiations env t.ty in
    match mapLookup t.ident env.funEnv with Some instEntry then
      -- NOTE(larshum, 2023-08-03): For each possible type instantiation of
      -- this variable, we instantiate the type of the variable (it may be
      -- polymorphic), extract the type parameter instantiation this
      -- monomorphic type represents, and add this instantiation to the entry
      -- of the function.
      let updatedInstMap =
        foldl
          (lam instMap. lam inst.
            let monoType = instantiatePolymorphicType inst t.ty in
            let funTypeInst = findTypeInstantiation instEntry.polyType monoType in
            if mapMem funTypeInst instMap then instMap
            else mapInsert funTypeInst (nameSetNewSym t.ident) instMap)
          instEntry.map instantiations
      in
      let instEntry = {instEntry with map = updatedInstMap} in
      {env with funEnv = mapInsert t.ident instEntry env.funEnv}
    else
      env
  | TmLet t ->
    let env = collectInstantiationsExpr instantiations env t.inexpr in
    let env = collectInstantiationsType instantiations env t.tyAnnot in
    let env = collectInstantiationsType instantiations env t.tyBody in
    let env = collectInstantiationsType instantiations env t.ty in
    let instantiations =
      match mapLookup t.ident env.funEnv with Some instEntry then
        -- NOTE(larshum, 2023-08-03): For the body of the let-expression, we
        -- combine all instantiations of the outer variables with the possible
        -- instantiations of the type variables bound in the current
        -- let-binding.
        let innerInst = mapKeys instEntry.map in
        join
          (map
            (lam outerInst. map (mapUnion outerInst) innerInst)
            instantiations)
      else
        instantiations
    in
    collectInstantiationsExpr instantiations env t.body
  | TmConDef t ->
    let env = collectInstantiationsExpr instantiations env t.inexpr in
    let env = collectInstantiationsType instantiations env t.ty in
    match mapLookup t.ident env.conEnv with Some conInstEntry then
      -- NOTE(larshum, 2023-08-03): We propagate the monomorphic instantiations
      -- of the constructor to its variant type.
      let variantId = findVariantName t.tyIdent in
      match mapLookup variantId env.typeEnv with Some varInstEntry then
        let updatedInstMap =
          mapFoldWithKey
            (lam instMap. lam inst. lam.
              let conMonoType = instantiatePolymorphicType inst t.tyIdent in
              let inst =
                match inspectType conMonoType with TyArrow {to = variantTy} then
                  findTypeInstantiation varInstEntry.polyType variantTy
                else
                  errorSingle [t.info] "Monomorphization error: Invalid constructor type"
              in
              if mapMem inst instMap then instMap
              else mapInsert inst (nameSetNewSym variantId) instMap)
            varInstEntry.map conInstEntry.map
        in
        let varInstEntry = {varInstEntry with map = updatedInstMap} in
        {env with typeEnv = mapInsert variantId varInstEntry env.typeEnv}
      else
        errorSingle [t.info] "Monomorphization error: Unknown variant type of constructor"
    else env
  | TmConApp t ->
    let env = collectInstantiationsType instantiations env t.ty in
    let env =
      match mapLookup t.ident env.conEnv with Some instEntry then
        let updatedInstMap =
          foldl
            (lam instMap. lam inst.
              let ty = tyarrow_ (tyTm t.body) t.ty in
              let monoType = instantiatePolymorphicType inst ty in
              let conTypeInst = findTypeInstantiation instEntry.polyType monoType in
              if mapMem conTypeInst instMap then instMap
              else mapInsert conTypeInst (nameSetNewSym t.ident) instMap)
            instEntry.map instantiations
        in
        let instEntry = {instEntry with map = updatedInstMap} in
        {env with conEnv = mapInsert t.ident instEntry env.conEnv}
      else env
    in
    collectInstantiationsExpr instantiations env t.body
  | ast ->
    let env = sfold_Expr_Expr (collectInstantiationsExpr instantiations) env ast in
    let env = sfold_Expr_Type (collectInstantiationsType instantiations) env ast in
    let env = sfold_Expr_TypeLabel (collectInstantiationsType instantiations) env ast in
    let env = sfold_Expr_Pat (collectInstantiationsPat instantiations) env ast in
    collectInstantiationsType instantiations env (tyTm ast)

  sem collectInstantiationsPat : [Instantiation] -> MonoEnv -> Pat -> MonoEnv
  sem collectInstantiationsPat instantiations env =
  | PatCon t ->
    let env =
      match mapLookup t.ident env.conEnv with Some instEntry then
        let updatedInstMap =
          foldl
            (lam instMap. lam inst.
              let ty = tyarrow_ (tyPat t.subpat) t.ty in
              let monoType = instantiatePolymorphicType inst ty in
              let conTypeInst = findTypeInstantiation instEntry.polyType monoType in
              if mapMem conTypeInst instMap then instMap
              else mapInsert conTypeInst (nameSetNewSym t.ident) instMap)
            instEntry.map instantiations
        in
        let instEntry = {instEntry with map = updatedInstMap} in
        {env with conEnv = mapInsert t.ident instEntry env.conEnv}
      else env
    in
    let env = collectInstantiationsPat instantiations env t.subpat in
    collectInstantiationsType instantiations env t.ty
  | p ->
    let env = sfold_Pat_Pat (collectInstantiationsPat instantiations) env p in
    collectInstantiationsType instantiations env (tyPat p)

  sem collectInstantiationsType : [Instantiation] -> MonoEnv -> Type -> MonoEnv
  sem collectInstantiationsType instantiations env =
  | TyAlias t ->
    -- NOTE(larshum, 2023-08-03): We collect instantiations of polymorphic
    -- aliases through occurrences of the type, as their use in expressions or
    -- patterns are implicit.
    match collectTypeAppParams [] t.display with (TyCon {ident = id}, ![]) then
      match mapLookup id env.typeEnv with Some instEntry then
        let updatedInstMap =
          foldl
            (lam instMap. lam inst.
              let ty = instantiatePolymorphicType inst t.display in
              if isMonomorphicTypeX true ty then
                let aliasTypeInst = findTypeInstantiation instEntry.polyType ty in
                if mapMem aliasTypeInst instMap then instMap
                else mapInsert aliasTypeInst (nameSetNewSym id) instMap
              else instMap)
            instEntry.map instantiations
        in
        let instEntry = {instEntry with map = updatedInstMap} in
        {env with typeEnv = mapInsert id instEntry env.conEnv}
      else
        -- NOTE(larshum, 2023-08-07): If the aliased type does not have an
        -- entry, it must be monomorphic.
        env
    else
      let info = infoTy t.display in
      errorSingle [info] "Monomorphization error: Unexpected shape of type alias"
  | ty -> sfold_Type_Type (collectInstantiationsType instantiations) env ty

  sem isMonomorphicTypeX : Bool -> Type -> Bool
  sem isMonomorphicTypeX acc =
  | TyAll _ | TyVar _ -> false
  | ty -> sfold_Type_Type isMonomorphicTypeX acc ty

  sem findVariantName : Type -> Name
  sem findVariantName =
  | TyAll t -> findVariantName t.ty
  | TyApp t -> findVariantName t.lhs
  | TyArrow t -> findVariantName t.to
  | TyCon t -> t.ident
  | ty -> errorSingle [infoTy ty] "Monomorphization error: Constructor type does not refer to a valid variant type"
end

lang MonomorphizeApply = MonomorphizeInstantiate + MonomorphizeResymbolize
  -- Replaces polymorphic constructs with their monomorphic bindings
  -- (bottom-up).
  sem applyMonomorphization : MonoEnv -> Expr -> Expr
  sem applyMonomorphization env =
  | ast -> applyMonomorphizationExpr env ast

  sem applyMonomorphizationExpr : MonoEnv -> Expr -> Expr
  sem applyMonomorphizationExpr env =
  | TmVar t ->
    let ident =
      match mapLookup t.ident env.funEnv with Some instEntry then
        let varInst = findTypeInstantiation instEntry.polyType t.ty in
        match mapLookup varInst instEntry.map with Some newId then
          newId
        else
          errorSingle [t.info] "Monomorphization error: Variable instantiation not found"
      else t.ident
    in
    TmVar {t with ident = ident,
                  ty = applyMonomorphizationType env t.ty}
  | TmLet t ->
    let inexpr = applyMonomorphizationExpr env t.inexpr in
    match mapLookup t.ident env.funEnv with Some instEntry then
      -- NOTE(larshum, 2023-08-03): The let-binding is a polymorphic function.
      -- We create once instance for each instantiation stored the entry.
      mapFoldWithKey
        (lam acc. lam inst. lam newId.
          let body = monomorphizeBody env inst t.body in
          let tyAnnot = monomorphizeType env inst t.tyAnnot in
          let tyBody = monomorphizeType env inst t.tyBody in
          let ty = monomorphizeType env inst t.ty in
          TmLet {
            ident = newId, tyAnnot = tyAnnot, tyBody = tyBody,
            body = body, inexpr = acc, ty = tyTm acc, info = t.info})
        inexpr instEntry.map
    else
      -- NOTE(larshum, 2023-08-03): The let-binding is already monomorphic, so
      -- we recurse directly into its body.
      TmLet {t with tyAnnot = applyMonomorphizationType env t.tyAnnot,
                    tyBody = applyMonomorphizationType env t.tyBody,
                    body = applyMonomorphization env t.body,
                    inexpr = inexpr,
                    ty = applyMonomorphizationType env t.ty}
  | TmType t ->
    let inexpr = applyMonomorphizationExpr env t.inexpr in
    match mapLookup t.ident env.typeEnv with Some instEntry then
      mapFoldWithKey
        (lam acc. lam inst. lam newId.
          let tyIdent = monomorphizeType env inst t.tyIdent in
          let ty = monomorphizeType env inst t.ty in
          TmType {
            ident = newId, params = [], tyIdent = tyIdent,
            inexpr = acc, ty = ty, info = t.info })
        inexpr instEntry.map
    else
        TmType {t with tyIdent = applyMonomorphizationType env t.tyIdent,
                       inexpr = inexpr,
                       ty = applyMonomorphizationType env t.ty}
  | TmConDef t ->
    let inexpr = applyMonomorphizationExpr env t.inexpr in
    match mapLookup t.ident env.conEnv with Some instEntry then
      mapFoldWithKey
        (lam acc. lam inst. lam newId.
          let tyIdent = monomorphizeType env inst t.tyIdent in
          let ty = monomorphizeType env inst t.ty in
          TmConDef {
            ident = newId, tyIdent = tyIdent, inexpr = acc,
            ty = ty, info = t.info })
        inexpr instEntry.map
    else
      TmConDef {t with tyIdent = applyMonomorphizationType env t.tyIdent,
                       inexpr = inexpr,
                       ty = applyMonomorphizationType env t.ty}
  | TmConApp t ->
    let ident =
      match mapLookup t.ident env.conEnv with Some instEntry then
        let conTy = tyarrow_ (tyTm t.body) t.ty in
        let conInst = findTypeInstantiation instEntry.polyType conTy in
        match mapLookup conInst instEntry.map with Some newId then
          newId
        else
          errorSingle [t.info] "Monomorphization error: Constructor instantiation not found"
      else t.ident
    in
    TmConApp {t with ident = ident,
                     ty = applyMonomorphizationType env t.ty}
  | ast ->
    let ast = smap_Expr_Expr (applyMonomorphizationExpr env) ast in
    let ast = smap_Expr_Pat (applyMonomorphizationPat env) ast in
    let ast = smap_Expr_Type (applyMonomorphizationType env) ast in
    let ast = smap_Expr_TypeLabel (applyMonomorphizationType env) ast in
    withType (applyMonomorphizationType env (tyTm ast)) ast

  sem applyMonomorphizationPat : MonoEnv -> Pat -> Pat
  sem applyMonomorphizationPat env =
  | PatCon t ->
    let subpat = applyMonomorphizationPat env t.subpat in
    match mapLookup t.ident env.conEnv with Some instEntry then
      let conType = TyArrow {from = tyPat t.subpat, to = t.ty, info = t.info} in
      let inst = findTypeInstantiation instEntry.polyType conType in
      match mapLookup inst instEntry.map with Some newId then
        PatCon {t with ident = newId, subpat = subpat}
      else
        error "Monomorphization error: Invalid constructor pattern instantiation"
    else
      PatCon {t with subpat = subpat}
  | p ->
    let p = smap_Pat_Pat (applyMonomorphizationPat env) p in
    withTypePat (applyMonomorphizationType env (tyPat p)) p

  sem applyMonomorphizationType : MonoEnv -> Type -> Type
  sem applyMonomorphizationType env =
  | (TyApp _) & ty ->
    match collectTypeAppParams [] ty with (TyCon t, ![]) then
      match mapLookup t.ident env.typeEnv with Some instEntry then
        let typeInst = findTypeInstantiation instEntry.polyType ty in
        match mapLookup typeInst instEntry.map with Some newId then
          TyCon {ident = newId, info = infoTy ty}
        else
          errorSingle [t.info] "Monomorphization error: Invalid constructor instantiation"
      else
        errorSingle [t.info] "Monomorphization error: Polymorphic constructor not found"
    else
      smap_Type_Type (applyMonomorphizationType env) ty
  | ty -> smap_Type_Type (applyMonomorphizationType env) ty

  sem monomorphizeBody : MonoEnv -> Instantiation -> Expr -> Expr
  sem monomorphizeBody env instantiation =
  | body ->
    let body = instantiatePolymorphicExpr instantiation body in
    let body = applyMonomorphizationExpr env body in
    resymbolizeBindings body

  sem monomorphizeType : MonoEnv -> Instantiation -> Type -> Type
  sem monomorphizeType env instantiation =
  | ty ->
    let ty = instantiatePolymorphicType instantiation ty in
    applyMonomorphizationType env ty
end

lang MExprMonomorphize = MonomorphizeCollect + MonomorphizeApply
  sem monomorphize : Expr -> Expr
  sem monomorphize =
  | ast ->
    let env = collectInstantiations ast in
    applyMonomorphization env ast
end

lang MExprMonomorphizeTest =
  MExprMonomorphize + MExprSym + MExprTypeCheck + MExprEq + MExprPrettyPrint +
  MExprEval

  -- Verifies that all types of the provided AST are monomorphic, i.e., that
  -- the AST does not contain any type variables or forall quantifiers.
  sem isMonomorphic : Expr -> Bool
  sem isMonomorphic =
  | ast -> isMonomorphicExpr true ast

  sem isMonomorphicExpr : Bool -> Expr -> Bool
  sem isMonomorphicExpr acc =
  | e ->
    let acc = sfold_Expr_Expr isMonomorphicExpr acc e in
    let acc = sfold_Expr_Pat isMonomorphicPat acc e in
    let acc = sfold_Expr_Type isMonomorphicType acc e in
    let acc = sfold_Expr_TypeLabel isMonomorphicType acc e in
    isMonomorphicType acc (tyTm e)

  sem isMonomorphicPat : Bool -> Pat -> Bool
  sem isMonomorphicPat acc =
  | p ->
    let acc = sfold_Pat_Pat isMonomorphicPat acc p in
    isMonomorphicType acc (tyPat p)

  sem isMonomorphicType : Bool -> Type -> Bool
  sem isMonomorphicType acc =
  | TyAll _ | TyVar _ -> false
  | ty -> sfold_Type_Type isMonomorphicType acc ty

  -- Verifies that all symbols introduced in the provided AST are distinct. We
  -- use this in our test suite to ensure that monomorphization resymbolizes
  -- all duplicated definitions.
  sem distinctSymbols : Expr -> Bool
  sem distinctSymbols =
  | ast -> distinctSymbolsExpr (setEmpty nameCmp) true ast

  sem distinctSymbolsExpr : Set Name -> Bool -> Expr -> Bool
  sem distinctSymbolsExpr syms acc =
  | TmLam t ->
    if setMem t.ident syms then false
    else distinctSymbolsExpr (setInsert t.ident syms) acc t.body
  | TmLet t ->
    if setMem t.ident syms then false
    else
      let acc = distinctSymbolsExpr syms acc t.body in
      distinctSymbolsExpr (setInsert t.ident syms) acc t.inexpr
  | TmRecLets t ->
    if any (lam bind. setMem bind.ident syms) t.bindings then false
    else
      let syms = foldl (lam syms. lam bind. setInsert bind.ident syms) syms t.bindings in
      let acc = foldl (lam acc. lam bind. distinctSymbolsExpr syms acc bind.body) acc t.bindings in
      distinctSymbolsExpr syms acc t.inexpr
  | TmType t ->
    if setMem t.ident syms then false
    else distinctSymbolsExpr (setInsert t.ident syms) acc t.inexpr
  | TmConDef t ->
    if setMem t.ident syms then false
    else distinctSymbolsExpr (setInsert t.ident syms) acc t.inexpr
  | TmExt t ->
    if setMem t.ident syms then false
    else distinctSymbolsExpr (setInsert t.ident syms) acc t.inexpr
  | t -> sfold_Expr_Expr (distinctSymbolsExpr syms) acc t
end

mexpr

use MExprMonomorphizeTest in

let preprocess =lam ast.
  typeCheck (symbolize ast)
in

-- Monomorphic function example
let monoFun = preprocess (bindall_ [
  ulet_ "addOne" (ulam_ "x" (addi_ (var_ "x") (int_ 1))),
  utuple_ [app_ (var_ "addOne") (int_ 1), app_ (var_ "addOne") (int_ 2)]
]) in
utest isMonomorphic monoFun with true in
let result = monomorphize monoFun in
utest isMonomorphic result with true in
utest monoFun with result using eqExpr in

-- Polymorphic identity function
let id = nameSym "id" in
let polyIdentity = preprocess (bindall_ [
  nulet_ id (ulam_ "x" (var_ "x")),
  utuple_ [app_ (nvar_ id) (int_ 2), app_ (nvar_ id) (float_ 2.5)]
]) in
let env = collectInstantiations polyIdentity in
utest mapSize env.funEnv with 1 in
utest mapMem id env.funEnv with true in
let result = applyMonomorphization env polyIdentity in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
let expected = preprocess (bindall_ [
  ulet_ "id_float" (ulam_ "x" (var_ "x")),
  ulet_ "id_int" (ulam_ "x" (var_ "x")),
  utuple_ [app_ (var_ "id_int") (int_ 2), app_ (var_ "id_float") (float_ 2.5)]
]) in
utest result with expected using eqExpr in

-- Unused function variable
let unused = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (int_ 0)),
  create_ (int_ 2) (var_ "f")
]) in
let env = collectInstantiations unused in
utest mapSize env.funEnv with 1 in
let result = applyMonomorphization env unused in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
utest eval ({env = evalEnvEmpty ()}) result with seq_ [int_ 0, int_ 0] using eqExpr in

-- Dependent polymorphic functions
let f = nameSym "f" in
let g = nameSym "g" in
let seqPoly = preprocess (bindall_ [
  nulet_ f (ulam_ "x" (var_ "x")),
  nulet_ g (ulam_ "x" (app_ (nvar_ f) (var_ "x"))),
  utuple_ [app_ (nvar_ g) (int_ 2), app_ (nvar_ f) (float_ 2.5)]
]) in
let env = collectInstantiations seqPoly in
utest mapSize env.funEnv with 2 in
utest mapMem f env.funEnv with true in
utest mapMem g env.funEnv with true in
let result = applyMonomorphization env seqPoly in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
let expected = preprocess (bindall_ [
  ulet_ "f_float" (ulam_ "x" (var_ "x")),
  ulet_ "f_int" (ulam_ "x" (var_ "x")),
  ulet_ "g_int" (ulam_ "x" (app_ (var_ "f_int") (var_ "x"))),
  utuple_ [app_ (var_ "g_int") (int_ 2), app_ (var_ "f_float") (float_ 2.5)]
]) in
utest result with expected using eqExpr in

-- Nested polymorphism where the inner function is polymorphic with the same
-- type as the outer function.
let h = nameSym "h" in
let nestedOuterPoly = preprocess (bindall_ [
  nulet_ f (ulam_ "g" (ulam_ "s" (bindall_ [
    nulet_ h (ulam_ "x" (app_ (var_ "g") (var_ "x"))),
    map_ (nvar_ h) (var_ "s")
  ]))),
  ulet_ "addOne" (ulam_ "x" (addi_ (var_ "x") (int_ 1))),
  ulet_ "addHalf" (ulam_ "x" (addf_ (var_ "x") (float_ 0.5))),
  utuple_ [
    appf2_ (nvar_ f) (var_ "addOne") (seq_ [int_ 2]),
    appf2_ (nvar_ f) (var_ "addHalf") (seq_ [float_ 2.5])
  ]
]) in
let env = collectInstantiations nestedOuterPoly in
utest mapSize env.funEnv with 1 in
utest mapMem f env.funEnv with true in
let result = applyMonomorphization env nestedOuterPoly in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
let expected = preprocess (bindall_ [
  let_ "f_float"
    (tyarrows_ [tyarrow_ tyfloat_ tyfloat_, tyseq_ tyfloat_, tyseq_ tyfloat_])
    (ulam_ "g" (ulam_ "s" (bindall_ [
      ulet_ "h" (ulam_ "x" (app_ (var_ "g") (var_ "x"))),
      map_ (var_ "h") (var_ "s")
  ]))),
  let_ "f_int"
    (tyarrows_ [tyarrow_ tyint_ tyint_, tyseq_ tyint_, tyseq_ tyint_])
    (ulam_ "g" (ulam_ "s" (bindall_ [
      ulet_ "h" (ulam_ "x" (app_ (var_ "g") (var_ "x"))),
      map_ (var_ "h") (var_ "s")
  ]))),
  ulet_ "addOne" (ulam_ "x" (addi_ (var_ "x") (int_ 1))),
  ulet_ "addHalf" (ulam_ "x" (addf_ (var_ "x") (float_ 0.5))),
  utuple_ [
    appf2_ (var_ "f_int") (var_ "addOne") (seq_ [int_ 2]),
    appf2_ (var_ "f_float") (var_ "addHalf") (seq_ [float_ 2.5])
  ]
]) in
utest result with expected using eqExpr in

-- Polymorphism in both functions, but it is only used in the inner one (i.e.,
-- only the inner one should be specialized).
let innerPoly = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (ulam_ "y" (bindall_ [
    nulet_ g (ulam_ "z" (var_ "z")),
    utuple_ [app_ (nvar_ g) (var_ "x"), app_ (nvar_ g) (var_ "y")]
  ]))),
  appf2_ (var_ "f") (int_ 2) (float_ 2.5)
]) in
let env = collectInstantiations innerPoly in
utest mapSize env.funEnv with 2 in
utest mapMem g env.funEnv with true in
let result = applyMonomorphization env innerPoly in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
let expected = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (ulam_ "y" (bindall_ [
    ulet_ "g_float" (ulam_ "z" (var_ "z")),
    ulet_ "g_int" (ulam_ "z" (var_ "z")),
    utuple_ [app_ (var_ "g_int") (var_ "x"), app_ (var_ "g_float") (var_ "y")]
  ]))),
  appf2_ (var_ "f") (int_ 2) (float_ 2.5)
]) in
utest result with expected using eqExpr in

-- Nested polymorphism where the inner and outer functions are polymorphic over
-- different type variables. In this case, we will end up with two versions of
-- the outer functions, and each of these contain two versions of the inner
-- function.
let nestedPoly = preprocess (bindall_ [
  nulet_ f (ulam_ "x" (ulam_ "y" (bindall_ [
    nulet_ g (ulam_ "z" (var_ "z")),
    utuple_ [app_ (nvar_ g) (var_ "x"), app_ (nvar_ g) (var_ "y")]
  ]))),
  utuple_ [
    appf2_ (nvar_ f) (int_ 2) (float_ 2.5),
    appf2_ (nvar_ f) (float_ 2.5) (int_ 2)
  ]
]) in
let env = collectInstantiations nestedPoly in
utest mapSize env.funEnv with 2 in
utest mapMem f env.funEnv with true in
utest mapMem g env.funEnv with true in
let result = applyMonomorphization env nestedPoly in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
let expected = preprocess (bindall_ [
-- why do these two seem to be unstable? should be the same outcome every time...
  ulet_ "f_int_float" (ulam_ "x" (ulam_ "y" (bindall_ [
    ulet_ "g_float" (ulam_ "z" (var_ "z")),
    ulet_ "g_int" (ulam_ "z" (var_ "z")),
    utuple_ [app_ (var_ "g_int") (var_ "x"), app_ (var_ "g_float") (var_ "y")]
  ]))),
  ulet_ "f_float_int" (ulam_ "x" (ulam_ "y" (bindall_ [
    ulet_ "g_float" (ulam_ "z" (var_ "z")),
    ulet_ "g_int" (ulam_ "z" (var_ "z")),
    utuple_ [app_ (var_ "g_float") (var_ "x"), app_ (var_ "g_int") (var_ "y")]
  ]))),
  utuple_ [
    appf2_ (var_ "f_int_float") (int_ 2) (float_ 2.5),
    appf2_ (var_ "f_float_int") (float_ 2.5) (int_ 2)
  ]
]) in
utest result with expected using eqExpr in

-- Polymorphic type constructor
let polyOption = preprocess (bindall_ [
  type_ "Option" ["a"] (tyvariant_ []),
  condef_ "Some" (tyall_ "a" (tyarrow_ (tyvar_ "a") (tyapp_ (tycon_ "Option") (tyvar_ "a")))),
  condef_ "None" (tyall_ "a" (tyarrow_ tyunit_ (tyapp_ (tycon_ "Option") (tyvar_ "a")))),
  ulet_ "isSome" (ulam_ "o" (
    match_ (var_ "o") (pcon_ "Some" pvarw_) true_ false_
  )),
  seq_ [
    app_ (var_ "isSome") (conapp_ "Some" (int_ 2)),
    app_ (var_ "isSome") (conapp_ "Some" (float_ 2.5)),
    app_ (var_ "isSome") (conapp_ "None" uunit_)
  ]
]) in
let env = collectInstantiations polyOption in
utest mapSize env.funEnv with 1 in
utest mapSize env.conEnv with 2 in
utest mapSize env.typeEnv with 1 in
let result = applyMonomorphization env polyOption in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
utest eval {env = evalEnvEmpty ()} result with seq_ [true_, true_, false_] using eqExpr in

-- Polymorphic type alias
let polyAlias = preprocess (bindall_ [
  type_ "Pair" ["a", "b"] (tytuple_ [tyvar_ "a", tyvar_ "b"]),
  let_ "fst"
    (tyall_ "a" (tyall_ "b"
      (tyarrow_ (tyapps_ (tycon_ "Pair") [tyvar_ "a", tyvar_ "b"]) (tyvar_ "a"))))
    (ulam_ "p" (tupleproj_ 0 (var_ "p"))),
  let_ "snd"
    (tyall_ "a" (tyall_ "b"
      (tyarrow_ (tyapps_ (tycon_ "Pair") [tyvar_ "a", tyvar_ "b"]) (tyvar_ "b"))))
    (ulam_ "p" (tupleproj_ 1 (var_ "p"))),
  ulet_ "x" (utuple_ [int_ 2, float_ 2.5]),
  ulet_ "y" (utuple_ [float_ 2.5, int_ 2]),
  seq_ [app_ (var_ "fst") (var_ "x"), app_ (var_ "snd") (var_ "y")]
]) in
let env = collectInstantiations polyAlias in
utest mapSize env.typeEnv with 1 in
let result = applyMonomorphization env polyAlias in
utest isMonomorphic result with true in
utest distinctSymbols result with true in
utest eval {env = evalEnvEmpty ()} result with seq_ [int_ 2, int_ 2] using eqExpr in

()
