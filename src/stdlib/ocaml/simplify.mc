include "mexpr/side-effect.mc"
include "ocaml/ast.mc"
include "map.mc"
include "name.mc"

-- Collects the parameters of an application and returns them in a tuple
-- together with the target expression (the function being called).
let collectAppArguments : use Ast in Expr -> (Expr, [Expr]) =
  use MExprAst in
  lam e.
  recursive let work = lam acc. lam e.
    match e with TmApp {lhs = !(TmApp _) & lhs, rhs = rhs} then
      (lhs, cons rhs acc)
    else match e with TmApp t then
      work (cons t.rhs acc) t.lhs
    else (e, acc)
  in
  work [] e

lang OCamlReplaceRecords = MExprAst
  syn Expr =
  | TmTuple {elems : [Expr], ty : Type, info : Info}

  sem tyTm : Expr -> Type
  sem tyTm =
  | TmTuple t -> t.ty

  sem withType : Type -> Expr -> Expr
  sem withType ty =
  | TmTuple t -> TmTuple {t with ty = ty}

  sem infoTm : Expr -> Info
  sem infoTm =
  | TmTuple t -> t.info

  sem smapAccumL_Expr_Expr : all acc. (acc -> Expr -> (acc, Expr)) -> acc -> Expr -> (acc, Expr)
  sem smapAccumL_Expr_Expr f acc =
  | TmTuple t ->
    match mapAccumL f acc t.elems with (acc, elems) in
    (acc, TmTuple {t with elems = elems})

  syn Pat =
  | PatTuple {pats : [Pat], ty : Type, info : Info}

  sem tyPat : Pat -> Type
  sem tyPat =
  | PatTuple t -> t.ty

  sem withTypePat : Type -> Pat -> Pat
  sem withTypePat ty =
  | PatTuple t -> PatTuple {t with ty = ty}

  syn Type =
  | TyTuple {tys : [Type], info : Info}

  sem replaceRecords : Expr -> Expr
  sem replaceRecords =
  | TmRecord t ->
    let ty = replaceRecordsType t.ty in
    TmTuple {elems = mapValues t.bindings, ty = ty, info = t.info}
  | t & (TmRecordUpdate _) ->
    recursive let collectInnerUpdates = lam kvs. lam t.
      match t with TmRecordUpdate tt then
        collectInnerUpdates (cons (tt.key, tt.value) kvs) tt.rec
      else (t, kvs)
    in
    match collectInnerUpdates [] t with (rec, kvs) in
    let i = infoTm t in
    let binds = extractBindings (unwrapType (tyTm rec)) in
    let bindKeys = mapKeys binds in

    -- If we have a record r : {x: Float, y: Float, z: Float} and we do
    --
    --   {r with x = 2.5}
    --
    -- we translate this into:
    --
    --   let (_, y, z) = r in
    --   (2.5, y, z)
    let updIndices : Map Int Expr =
      mapFromSeq
        subi
        (map
          (lam kv.
            match kv with (k, v) in
            match findi (eqSID k) bindKeys with Some idx then
              (idx, v)
            else error "")
          kvs)
    in
    let boundIds =
      create
        (length bindKeys)
        (lam i.
          if mapMem i updIndices then None () else Some (nameSym "_var"))
    in
    let pats =
      create
        (length bindKeys)
        (lam i.
          let ident =
            match get boundIds i with Some id then
              PName id
            else
              PWildcard ()
          in
          PatNamed {ident = ident, ty = TyUnknown {info = NoInfo ()}, info = NoInfo ()})
    in
    let tms =
      create
        (length bindKeys)
        (lam i.
          match mapLookup i updIndices with Some v then v
          else match get boundIds i with Some id then nvar_ id
          else never)
    in
    let elems = map replaceRecords tms in
    let ty = replaceRecordsType (tyTm t) in
    TmMatch {
      target = rec,
      pat = PatTuple {pats = pats, ty = ty, info = i},
      thn = TmTuple {elems = elems, ty = ty, info = i},
      els = TmNever {ty = TyUnknown {info = i}, info = i},
      ty = ty,
      info = i
    }
  | t ->
    let t = smap_Expr_Type replaceRecordsType t in
    let t = smap_Expr_Pat replaceRecordsPat t in
    smap_Expr_Expr replaceRecords t

  sem replaceRecordsPat : Pat -> Pat
  sem replaceRecordsPat =
  | PatRecord t ->
    let binds = extractBindings (unwrapType t.ty) in
    let ty = replaceRecordsType t.ty in
    let pats =
      mapMapWithKey
        (lam id. lam.
          match mapLookup id t.bindings with Some p then
            p
          else
            PatNamed {ident = PWildcard (), info = t.info, ty = TyInt {info = t.info}})
        binds
    in
    PatTuple {pats = mapValues pats, ty = ty, info = t.info}
  | p -> smap_Pat_Pat replaceRecordsPat p

  sem extractBindings : Type -> Map SID Type
  sem extractBindings =
  | TyRecord t -> t.fields
  | _ -> error "invalid type of pattern"

  sem replaceRecordsType : Type -> Type
  sem replaceRecordsType =
  | TyRecord t -> TyTuple {tys = mapValues t.fields, info = t.info}
  | ty -> smap_Type_Type replaceRecordsType ty
end

lang OCamlSimplify = OCamlAst + MExprSideEffect + OCamlReplaceRecords
  sem exprArity : SideEffectEnv -> Expr -> Int
  sem exprArity env =
  | OTmLam t -> addi (exprArity env t.body) 1
  | OTmVarExt _ -> 1
  | OTmExprExt _ -> 1

  sem simplify : [Top] -> [Top]
  sem simplify =
  | tops ->
    let env = mapEmpty nameCmp in
    match mapAccumL simplifyTop env tops with (_, tops) in
    let env = setEmpty nameCmp in
    match foldr (lam t. lam acc. deadcodeTop acc t) (env, []) tops with (_, tops) in
    let tops = inlineSingleUseBindings tops in
    let tops = removeRedundantObjMagics tops in
    simplifyThunksInObjMagic tops

  sem simplifyTop : Map Name Int -> Top -> (Map Name Int, Top)
  sem simplifyTop env =
  | t & (OTopTypeDecl _) -> (env, t)
  | OTopVariantTypeDecl t ->
    let nconstrs = mapSize t.constrs in
    let env = mapFoldWithKey (lam env. lam k. lam. mapInsert k nconstrs env) env t.constrs in
    (env, OTopVariantTypeDecl t)
  | t & (OTopCExternalDecl _) -> (env, t)
  | OTopLet t ->
    (env, OTopLet {t with body = simplifyExpr env t.body})
  | OTopRecLets t ->
    (env, OTopRecLets {t with bindings = map (simplifyTopBinding env) t.bindings})
  | OTopExpr t ->
    (env, OTopExpr {t with expr = simplifyExpr env t.expr})
  | OTopTryWith t ->
    (env, OTopTryWith {t with try = simplifyExpr env t.try,
                              arms = map (simplifyArm env) t.arms})

  sem simplifyTopBinding : Map Name Int -> OCamlTopBinding -> OCamlTopBinding
  sem simplifyTopBinding env =
  | t -> {t with body = simplifyExpr env t.body}

  sem simplifyArm : Map Name Int -> (Pat, Expr) -> (Pat, Expr)
  sem simplifyArm env =
  | (p, e) -> (p, simplifyExpr env e)

  sem simplifyExpr : Map Name Int -> Expr -> Expr
  sem simplifyExpr env =
  | OTmMatch t ->
    -- Remove redundant final case of a match expression (the never term reference)
    let arms = if hasRedundantNeverTerm env t.arms then init t.arms else t.arms in
    OTmMatch { target = simplifyExpr env t.target
             , arms = map (simplifyArm env) arms }
  | t -> smap_Expr_Expr (simplifyExpr env) t

  sem hasRedundantNeverTerm : Map Name Int -> [(Pat, Expr)] -> Bool
  sem hasRedundantNeverTerm env =
  | arms ->
    let f = lam arm.
      match arm with (OPatCon t, _) then
        match mapLookup t.ident env with Some n then n else -1
      else -1
    in
    if gti (length arms) 1 then
      let val = map f arms in
      let fst = head val in
      and (eqi (subi (length val) 1) fst) (forAll (eqi (head val)) (init val))
    else false

  sem deadcodeTop : (Set Name, [Top]) -> Top -> (Set Name, [Top])
  sem deadcodeTop acc =
  | OTopLet t ->
    match acc with (ids, tops) in
    if shouldKeepIdent ids t.ident t.body then
      match deadcodeExpr ids t.body with (ids, body) in
      (ids, cons (OTopLet {t with body = body}) tops)
    else
      (ids, tops)
  | OTopRecLets t ->
    let deadcodeBody = lam ids. lam bind.
      match deadcodeExpr ids bind.body with (ids, body) in
      (ids, {bind with body = body})
    in
    match acc with (ids, tops) in
    match mapAccumL deadcodeBody ids t.bindings with (ids, bindings) in
    let bindings = filter (lam bind. shouldKeepIdent ids bind.ident bind.body) bindings in
    (ids, cons (OTopRecLets {bindings = bindings}) tops)
  | OTopExpr t ->
    match acc with (ids, tops) in
    match deadcodeExpr ids t.expr with (ids, expr) in
    let tops = cons (OTopExpr {expr = expr}) tops in
    (ids, tops)
  | t ->
    match acc with (ids, tops) in
    (ids, cons t tops)

  sem deadcodeExpr : Set Name -> Expr -> (Set Name, Expr)
  sem deadcodeExpr ids =
  | TmVar t -> (setInsert t.ident ids, TmVar t)
  | TmDecl (t & {decl = DeclLet tt}) ->
    match deadcodeExpr ids t.inexpr with (ids, inexpr) in
    if shouldKeepIdent ids tt.ident tt.body then
      match deadcodeExpr ids tt.body with (ids, body) in
      let d = TmDecl {t with decl = DeclLet {tt with body = body},
                             inexpr = inexpr} in
      (ids, d)
    else
      (ids, inexpr)
  | TmDecl (t & {decl = DeclRecLets tt}) ->
    let deadcodeBody = lam ids. lam bind.
      match deadcodeExpr ids bind.body with (ids, body) in
      (ids, {bind with body = body})
    in
    match deadcodeExpr ids t.inexpr with (ids, inexpr) in
    match mapAccumL deadcodeBody ids tt.bindings with (ids, bindings) in
    let bindings = filter (lam bind. shouldKeepIdent ids bind.ident bind.body) bindings in
    (ids, TmDecl {t with decl = DeclRecLets {tt with bindings = bindings},
                         inexpr = inexpr})
  | t -> smapAccumL_Expr_Expr deadcodeExpr ids t

  sem shouldKeepIdent : Set Name -> Name -> Expr -> Bool
  sem shouldKeepIdent ids id =
  | body -> if setMem id ids then true else hasSideEffect body

  sem inlineSingleUseBindings : [Top] -> [Top]
  sem inlineSingleUseBindings =
  | tops -> map inlineSingleUseBindingsTop tops

  sem inlineSingleUseBindingsTop : Top -> Top
  sem inlineSingleUseBindingsTop =
  | OTopLet t -> OTopLet {t with body = inlineSingleUseBindingsExpr t.body}
  | OTopRecLets t ->
    let inlineSingleUseBindingsBind = lam bind.
      {bind with body = inlineSingleUseBindingsExpr bind.body}
    in
    OTopRecLets {t with bindings = map inlineSingleUseBindingsBind t.bindings}
  | t -> t

  sem inlineSingleUseBindingsExpr : Expr -> Expr
  sem inlineSingleUseBindingsExpr =
  | t ->
    let env = collectVariableUses (mapEmpty nameCmp) t in
    inlineSingleUseBindingsInExpr env (mapEmpty nameCmp) t

  sem collectVariableUses : Map Name Int -> Expr -> Map Name Int
  sem collectVariableUses env =
  | TmVar t -> mapInsertWith addi t.ident 1 env
  | t ->
    let env = sfold_Expr_Expr collectVariableUses env t in
    sfold_Expr_Pat collectVariableUsesPat env t

  sem collectVariableUsesPat : Map Name Int -> Pat -> Map Name Int
  sem collectVariableUsesPat env =
  | PatNamed t ->
    match t.ident with PName id then mapInsertWith addi id 1 env
    else env
  | p -> sfold_Pat_Pat collectVariableUsesPat env p

  sem inlineSingleUseBindingsInExpr : Map Name Int -> Map Name Expr -> Expr -> Expr
  sem inlineSingleUseBindingsInExpr env subMap =
  | TmVar t ->
    match mapLookup t.ident subMap with Some e then e else TmVar t
  | TmDecl (t & {decl = DeclLet tt, inexpr = inexpr}) ->
    let body = inlineSingleUseBindingsInExpr env subMap tt.body in
    match mapLookup tt.ident env with Some 1 then
      let subMap = mapInsert tt.ident body subMap in
      inlineSingleUseBindingsInExpr env subMap inexpr
    else
      let decl = DeclLet {tt with body = body} in
      TmDecl {t with decl = decl,
                     inexpr = inlineSingleUseBindingsInExpr env subMap inexpr}
  | t -> smap_Expr_Expr (inlineSingleUseBindingsInExpr env subMap) t

  sem removeRedundantObjMagics : [Top] -> [Top]
  sem removeRedundantObjMagics =
  | tops -> map removeRedundantObjMagicsTop tops

  sem removeRedundantObjMagicsTop : Top -> Top
  sem removeRedundantObjMagicsTop =
  | OTopLet t -> OTopLet {t with body = removeRedundantObjMagicsExpr t.body}
  | OTopRecLets t ->
    let removeBind = lam bind.
      {bind with body = removeRedundantObjMagicsExpr bind.body}
    in
    OTopRecLets {t with bindings = map removeBind t.bindings}
  | OTopExpr {expr = expr} ->
    OTopExpr {expr = removeRedundantObjMagicsExpr expr}
  | t -> t

  sem removeRedundantObjMagicsExpr : Expr -> Expr
  sem removeRedundantObjMagicsExpr =
  | TmApp (t & {lhs = OTmVarExt {ident = "Obj.magic"}}) ->
    match collectAppArguments t.rhs with (OTmVarExt {ident = "Obj.magic"}, _) then
      removeRedundantObjMagicsExpr t.rhs
    else
      TmApp {t with lhs = removeRedundantObjMagicsExpr t.lhs,
                    rhs = removeRedundantObjMagicsExpr t.rhs}
  | t -> smap_Expr_Expr removeRedundantObjMagicsExpr t

  sem simplifyThunksInObjMagic : [Top] -> [Top]
  sem simplifyThunksInObjMagic =
  | tops -> map simplifyThunksInObjMagicTop tops

  sem simplifyThunksInObjMagicTop : Top -> Top
  sem simplifyThunksInObjMagicTop =
  | OTopLet t -> OTopLet {t with body = simplifyThunksInObjMagicExpr t.body}
  | OTopRecLets t ->
    let simplifyBind = lam bind.
      {bind with body = simplifyThunksInObjMagicExpr bind.body}
    in
    OTopRecLets {t with bindings = map simplifyBind t.bindings}
  | OTopExpr {expr = expr} ->
    OTopExpr {expr = simplifyThunksInObjMagicExpr expr}
  | t -> t

  sem simplifyThunksInObjMagicExpr : Expr -> Expr
  sem simplifyThunksInObjMagicExpr =
  | t & (TmApp {
      lhs = OTmVarExt {ident = "Obj.magic"},
      rhs = TmApp (tt & {
        lhs = TmLam {body = body & !(TmLam _)},
        rhs = TmRecord {bindings = bindings}
      })
  }) ->
    if mapIsEmpty bindings then
      TmApp {tt with lhs = OTmVarExt {ident = "Obj.magic"},
                     rhs = simplifyThunksInObjMagicExpr body}
    else
      smap_Expr_Expr simplifyThunksInObjMagicExpr t
  | t & (TmApp {
      lhs = TmApp (tt & {
        lhs = OTmVarExt {ident = "Obj.magic"},
        rhs = TmLam {body = body & !(TmLam _)}}),
      rhs = TmRecord {bindings = bindings}
  }) ->
    if mapIsEmpty bindings then
      TmApp {tt with rhs = simplifyThunksInObjMagicExpr body}
    else
      smap_Expr_Expr simplifyThunksInObjMagicExpr t
  | t -> smap_Expr_Expr simplifyThunksInObjMagicExpr t
end
