include "./ast.mc"
include "map.mc"
include "name.mc"

lang OCamlSimplify = OCamlAst
  sem simplify : [Top] -> [Top]
  sem simplify =
  | tops ->
    let env = mapEmpty nameCmp in
    match mapAccumL simplifyTop env tops with (_, tops) in
    let env = setEmpty nameCmp in
    match foldr (lam t. lam acc. deadcodeTop acc t) (env, []) tops with (_, tops) in
    tops

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
    if setMem t.ident ids then
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
    -- TODO: remove unused recursive let-bindings
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
  | TmDecl (t & {decl = DeclLet tt, inexpr = inexpr}) ->
    match deadcodeExpr ids t.inexpr with (ids, inexpr) in
    if setMem tt.ident ids then
      match deadcodeExpr ids tt.body with (ids, body) in
      let d = TmDecl {t with decl = DeclLet {tt with body = body},
                             inexpr = inexpr} in
      (ids, d)
    else
      (ids, inexpr)
  | t -> smapAccumL_Expr_Expr deadcodeExpr ids t
end
