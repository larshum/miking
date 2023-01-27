-- Implementation of lambda lifting, which lifts out all nested function
-- definitions to the global scope. This implementation assumes that the
-- program has been type-checked, as it depends on the types to determine which
-- variables to capture.
--
-- Note that when free variables are captured in a function, its type changes.
-- Because of this, the lambda lifting will remove type annotations that are
-- invalidated after capturing variables.

include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/call-graph.mc"
include "mexpr/eq.mc"
include "mexpr/pprint.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-check.mc"
include "mexpr/utils.mc"

lang LambdaLift = MExprAst
  sem isFunctionType : Type -> Bool
  sem isFunctionType =
  | ty ->
    match inspectType ty with TyArrow _ then true
    else false
end

-- Add names to all anonymous lambdas. A lambda that is the immediate body of
-- a let- or recursive let-expression is not considered anonymous.
lang LambdaLiftNameAnonymous = LambdaLift
  sem nameAnonymousFunctions : Expr -> Expr
  sem nameAnonymousFunctions =
  | TmLam t & lambda ->
    let lambda = nameAnonymousFunctionsBody lambda in
    let id = nameSym "t" in
    TmLet {
      ident = id,
      tyAnnot = TyUnknown {info = t.info},
      tyBody = t.ty,
      body = lambda,
      inexpr = TmVar {ident = id, ty = t.ty, info = t.info, frozen = false},
      ty = t.ty, info = t.info}
  | TmLet t ->
    TmLet {t with body = nameAnonymousFunctionsBody t.body,
                  inexpr = nameAnonymousFunctions t.inexpr}
  | TmRecLets t ->
    let nameAnonymousFunctionsBinding = lam bind.
      {bind with body = nameAnonymousFunctionsBody bind.body}
    in
    TmRecLets {t with bindings = map nameAnonymousFunctionsBinding t.bindings,
                      inexpr = nameAnonymousFunctions t.inexpr}
  | t -> smap_Expr_Expr nameAnonymousFunctions t

  sem nameAnonymousFunctionsBody : Expr -> Expr
  sem nameAnonymousFunctionsBody =
  | TmLam t -> TmLam {t with body = nameAnonymousFunctionsBody t.body}
  | t -> nameAnonymousFunctions t
end

-- Captures the free type variables in a program. We run this step prior to the
-- capturing of expression variables so that we do not have to worry about
-- their types when capturing them.
lang LambdaLiftTypeVariables = LambdaLift

  sem collectBoundTypes : Map Name VarSort -> Type -> (Map Name VarSort, Type)
  sem collectBoundTypes bound =
  | TyAll t ->
    match collectBoundTypes bound t.ty with (bound, ty) in
    (mapInsert t.ident t.sort bound, ty)
  | ty -> (bound, ty)

  sem findFreeTypeVariablesExpr : Map Name VarSort -> Set Name
                               -> Map Name (Name, VarSort) -> Expr
                               -> Map Name (Name, VarSort)
  sem findFreeTypeVariablesExpr vars bound free =
  | TmLet t ->
    -- NOTE(larshum, 2023-01-23): We do not look for free type variables in the
    -- bodies of inner bindings. As we do a depth-first traversal, all inner
    -- bindings will already have bound all their free type variables.
    let free =
      if isFunctionType t.tyBody then free
      else findFreeTypeVariablesExpr vars bound free t.body
    in
    let free = findFreeTypeVariablesType vars bound free t.tyBody in
    findFreeTypeVariablesExpr vars bound free t.inexpr
  | TmRecLets t ->
    let findFreeTypeVariablesBinding = lam free. lam bind.
      findFreeTypeVariablesType vars bound free bind.tyBody
    in
    let free = foldl findFreeTypeVariablesBinding free t.bindings in
    findFreeTypeVariablesExpr vars bound free t.inexpr
  | t ->
    let free = sfold_Expr_Expr (findFreeTypeVariablesExpr vars bound) free t in
    let free = sfold_Expr_Type (findFreeTypeVariablesType vars bound) free t in
    let free = sfold_Expr_Pat (findFreeTypeVariablesPat vars bound) free t in
    sfold_Expr_TypeLabel (findFreeTypeVariablesType vars bound) free t

  sem findFreeTypeVariablesPat : Map Name VarSort -> Set Name
                              -> Map Name (Name, VarSort) -> Pat
                              -> Map Name (Name, VarSort)
  sem findFreeTypeVariablesPat vars bound free =
  | p ->
    let free = sfold_Pat_Pat (findFreeTypeVariablesPat vars bound) free p in
    sfold_Pat_Type (findFreeTypeVariablesType vars bound) free p

  sem findFreeTypeVariablesType : Map Name VarSort -> Set Name
                               -> Map Name (Name, VarSort) -> Type
                               -> Map Name (Name, VarSort)
  sem findFreeTypeVariablesType vars bound free =
  | TyAll t ->
    let bound = setInsert t.ident bound in
    findFreeTypeVariablesType vars bound free t.ty
  | TyVar t ->
    if setMem t.ident bound then free
    else match mapLookup t.ident vars with Some sort then
      mapInsert t.ident (nameSetNewSym t.ident, sort) free
    else
      errorSingle [t.info] "Unbound type variable encountered in lambda lifting"
  | ty -> sfold_Type_Type (findFreeTypeVariablesType vars bound) free ty

  sem findFreeTypeVariables : Map Name VarSort -> Expr -> Type
                           -> Map Name (Name, VarSort)
  sem findFreeTypeVariables vars body =
  | tyBody ->
    match collectBoundTypes (mapEmpty nameCmp) tyBody with (bound, innerTyBody) in
    let bound = mapMapWithKey (lam. lam. ()) bound in
    let fv = findFreeTypeVariablesType vars bound (mapEmpty nameCmp) innerTyBody in
    findFreeTypeVariablesExpr vars bound fv body

  sem insertCapturedTypeVariablesExpr : Map Name (Name, VarSort) -> Expr -> Expr
  sem insertCapturedTypeVariablesExpr captured =
  | t ->
    let t = smap_Expr_Expr (insertCapturedTypeVariablesExpr captured) t in
    let t = smap_Expr_Pat (insertCapturedTypeVariablesPat captured) t in
    let t = smap_Expr_Type (eraseFreeAnnotatedTypeVariables captured) t in
    smap_Expr_TypeLabel (insertCapturedTypeVariablesType captured) t

  sem eraseFreeAnnotatedTypeVariables : Map Name (Name, VarSort) -> Type -> Type
  sem eraseFreeAnnotatedTypeVariables captured =
  | TyVar t ->
    if mapMem t.ident captured then TyUnknown {info = t.info}
    else TyVar t
  | ty -> smap_Type_Type (eraseFreeAnnotatedTypeVariables captured) ty

  sem insertCapturedTypeVariablesPat : Map Name (Name, VarSort) -> Pat -> Pat
  sem insertCapturedTypeVariablesPat captured =
  | p ->
    let p = smap_Pat_Pat (insertCapturedTypeVariablesPat captured) p in
    smap_Pat_Type (insertCapturedTypeVariablesType captured) p

  sem insertCapturedTypeVariablesType : Map Name (Name, VarSort) -> Type -> Type
  sem insertCapturedTypeVariablesType captured =
  | TyVar t ->
    match mapLookup t.ident captured with Some (newId, _) then
      TyVar {t with ident = newId}
    else TyVar t
  | ty -> smap_Type_Type (insertCapturedTypeVariablesType captured) ty

  sem insertTypeVariableCaptures : Type -> [(Name, (Name, VarSort))] -> Type
  sem insertTypeVariableCaptures ty =
  | rest ++ [(_, (newId, sort))] ->
    let tyAll = TyAll { ident = newId, sort = sort, ty = ty, info = infoTy ty } in
    insertTypeVariableCaptures tyAll rest
  | [] -> ty

  sem bindCapturedTypeVariables : Map Name (Name, VarSort) -> Type -> Type
  sem bindCapturedTypeVariables captured =
  | tyBody ->
    let tyBody = insertCapturedTypeVariablesType captured tyBody in
    insertTypeVariableCaptures tyBody (mapBindings captured)

  sem captureTypeVariablesExpr : Map Name VarSort -> Expr -> Expr
  sem captureTypeVariablesExpr vars =
  | TmLet t ->
    -- Recursively capture free type variables in the body of the let-binding.
    match collectBoundTypes vars t.tyBody with (vars, _) in
    let t = {t with body = captureTypeVariablesExpr vars t.body} in

    -- If the binding is of a function type, capture free type variables in its
    -- body and bind them in the type of its body.
    let t =
      if isFunctionType t.tyBody then
        let fv = findFreeTypeVariables vars t.body t.tyBody in
        if mapIsEmpty fv then t
        else
          let tyAnnot = TyUnknown {info = infoTy t.tyAnnot} in
          let tyBody = bindCapturedTypeVariables fv t.tyBody in
          let body = insertCapturedTypeVariablesExpr fv t.body in
          {t with tyAnnot = tyAnnot, tyBody = tyBody, body = body}
      else t
    in
    TmLet {t with inexpr = captureTypeVariablesExpr vars t.inexpr}
  | TmRecLets t ->
    let captureTypeVariablesBinding = lam bind.
      match collectBoundTypes vars bind.tyBody with (vars, _) in
      let body = captureTypeVariablesExpr vars bind.body in
      let fv = findFreeTypeVariables vars body bind.tyBody in
      if mapIsEmpty fv then {bind with body = body}
      else
        let tyAnnot = TyUnknown {info = infoTy bind.tyAnnot} in
        let tyBody = bindCapturedTypeVariables fv bind.tyBody in
        let body = insertCapturedTypeVariablesExpr fv body in
        {bind with tyAnnot = tyAnnot, tyBody = tyBody, body = body}
    in
    TmRecLets {t with bindings = map captureTypeVariablesBinding t.bindings,
                      inexpr = captureTypeVariablesExpr vars t.inexpr}
  | t -> smap_Expr_Expr (captureTypeVariablesExpr vars) t

  sem captureTypeVariables : Expr -> Expr
  sem captureTypeVariables =
  | t -> captureTypeVariablesExpr (mapEmpty nameCmp) t
end

-- Initializes the lambda lifting state for a given program by finding the free
-- variables of all bindings by solving set equations.
lang LambdaLiftSolveEquations = LambdaLift + MExprCallGraph

  type LambdaLiftSolution = Map Name Type
  type LambdaLiftSolutions = Map Name LambdaLiftSolution

  type LambdaLiftState = {
    -- The set of bound variables in a program. These do not include functions,
    -- which are included in the solution map instead. Note that higher-order
    -- function parameters are included in this set.
    vars : Set Name,

    -- Contains the lambda lifting solution of a function. The solution maps
    -- the captured free variables to their type.
    sols : LambdaLiftSolutions
  }

  sem lambdaLiftStateEmpty : () -> LambdaLiftState
  sem lambdaLiftStateEmpty =
  | _ ->
    { vars = setEmpty nameCmp
    , sols = mapEmpty nameCmp }

  sem boundVariablesInPattern : Map Name Type -> Pat -> Map Name Type
  sem boundVariablesInPattern acc =
  | PatNamed {ident = PName id, ty = ty} -> mapInsert id ty acc
  | PatSeqEdge {prefix = prefix, middle = PName id, postfix = postfix, ty = ty} ->
    let acc = foldl boundVariablesInPattern acc prefix in
    let acc = foldl boundVariablesInPattern acc postfix in
    mapInsert id ty acc
  | p -> sfold_Pat_Pat boundVariablesInPattern acc p

  sem collectBoundVariablesInPattern : LambdaLiftState -> Pat -> LambdaLiftState
  sem collectBoundVariablesInPattern state =
  | p ->
    mapFoldWithKey
      (lam state. lam id. lam. {state with vars = setInsert id state.vars})
      state (boundVariablesInPattern (mapEmpty nameCmp) p)

  sem freeVariables : LambdaLiftState -> Expr -> Map Name Type
  sem freeVariables state =
  | t -> freeVariablesExpr state (mapEmpty nameCmp) (mapEmpty nameCmp) t

  sem freeVariablesExpr : LambdaLiftState -> Map Name Type -> Map Name Type
                       -> Expr -> Map Name Type
  sem freeVariablesExpr state bound free =
  | TmVar t ->
    if mapMem t.ident bound then free
    else if setMem t.ident state.vars then mapInsert t.ident t.ty free
    else match mapLookup t.ident state.sols with Some fv then
      mapUnion free fv
    else errorSingle [t.info] "Unbound variable encountered in lambda lifting"
  | TmLam t -> freeVariablesExpr state (mapInsert t.ident t.tyIdent bound) free t.body
  | TmLet t ->
    -- NOTE(larshum, 2023-01-22): If this binding is a function, then we
    -- already know its solution as we compute solutions in a depth-first
    -- fashion. As the solution consists of the free variables, we can simply
    -- reuse it directly, by removing the bound variables (which are bound
    -- outside of its scope).
    let free =
      match mapLookup t.ident state.sols with Some fv then
        mapUnion
          free
          (mapFoldWithKey (lam fv. lam id. lam. mapRemove id fv) fv bound)
      else if or (setMem t.ident state.vars) (mapMem t.ident bound) then
        freeVariablesExpr state bound free t.body
      else errorSingle [t.info] "Unbound variable encountered in lambda lifting"
    in
    let bound = mapInsert t.ident t.tyBody bound in
    freeVariablesExpr state bound free t.inexpr
  | TmRecLets t ->
    let bound =
      foldl
        (lam bound. lam bind. mapInsert bind.ident bind.tyBody bound)
        bound t.bindings
    in
    -- NOTE(larshum, 2023-01-22): We apply the same approach as for
    -- let-expressions. A recursive let-binding must always be a function, so
    -- the solutions must be available here due to our depth-first approach.
    let free =
      foldl
        (lam free. lam bind.
          match mapLookup bind.ident state.sols with Some fv then
            mapUnion
              free
              (mapFoldWithKey
                (lam fv. lam id. lam. mapRemove id fv)
                fv bound)
          else errorSingle [t.info] "Unbound variable encountered in lambda lifting")
        free t.bindings
    in
    freeVariablesExpr state bound free t.inexpr
  | TmMatch t ->
    let free = freeVariablesExpr state bound free t.target in
    let boundThn = boundVariablesInPattern bound t.pat in
    let free = freeVariablesExpr state boundThn free t.thn in
    freeVariablesExpr state bound free t.els
  | t -> sfold_Expr_Expr (freeVariablesExpr state bound) free t

  sem propagateFunNames : LambdaLiftState -> Digraph Name Int -> [[Name]]
                       -> LambdaLiftState
  sem propagateFunNames state g =
  | [scc] ++ t ->
    -- NOTE(larshum, 2023-01-22): The free variables for all bindings in the
    -- same strongly connected component is the union of their local free
    -- variables.
    let fv =
      foldl
        (lam acc. lam id.
          match mapLookup id state.sols with Some fv then
            mapUnion acc fv
          else acc)
        (mapEmpty nameCmp) scc
    in

    -- NOTE(larshum, 2023-01-22): Update the solutions to all bindings in this
    -- strongly connected component.
    let state =
      foldl
        (lam state. lam id. {state with sols = mapInsert id fv state.sols})
        state scc
    in
    propagateFunNames state g t
  | [] -> state

  sem solveSetEquations : LambdaLiftState -> Expr -> LambdaLiftState
  sem solveSetEquations state =
  | TmLam t ->
    let state = {state with vars = setInsert t.ident state.vars} in
    solveSetEquations state t.body
  | TmLet t ->
    let state = solveSetEquations state t.body in
    let state =
      -- NOTE(larshum, 2023-01-22): We make use of the type of the binding to
      -- determine whether it is a function or not. For this to work, we
      -- require type-checking to run prior to lambda lifting. The benefit is
      -- that we will not capture parameters of function type, which may cause
      -- type-related issues.
      if isFunctionType t.tyBody then
        let fv = freeVariables state t.body in
        {state with sols = mapInsert t.ident fv state.sols}
      else
        {state with vars = setInsert t.ident state.vars}
    in
    solveSetEquations state t.inexpr
  | TmRecLets t ->
    -- NOTE(larshum, 2023-01-22): Start off by inserting dummy solutions for
    -- the bindings, to ensure that they are all aware of each other's
    -- existence when finding their free variables.
    let state =
      foldl
        (lam state. lam bind.
          {state with sols = mapInsert bind.ident (mapEmpty nameCmp) state.sols})
        state t.bindings
    in

    -- NOTE(larshum, 2023-01-22): Solve the set equations for the bodies of the
    -- recursive bindings, and find their local free variables before looking
    -- for a fixed point.
    let state =
      foldl
        (lam state. lam bind.
          let state = solveSetEquations state bind.body in
          let fv = freeVariables state bind.body in
          {state with sols = mapInsert bind.ident fv state.sols})
        state t.bindings
    in

    -- NOTE(larshum, 2023-01-22): Find the fixed point solution to the set
    -- equations for the recursive bindings.
    let g = constructCallGraph (TmRecLets t) in
    let sccs = digraphTarjan g in
    let state = propagateFunNames state g (reverse sccs) in
    solveSetEquations state t.inexpr
  | TmMatch t ->
    let state = solveSetEquations state t.target in
    let state = collectBoundVariablesInPattern state t.pat in
    let state = solveSetEquations state t.thn in
    solveSetEquations state t.els
  | t -> sfold_Expr_Expr solveSetEquations state t
end

-- Captures the free variables of each function, to ensure they can be lifted
-- to the top of the program.
lang LambdaLiftCaptureVariables = LambdaLift + LambdaLiftSolveEquations

  -- Defines a state used for the capturing of free variables, parts of which
  -- are precomputed at the start of the 'captureFreeVariables' function
  -- based on the provided solutions to the set equations. The first two maps
  -- have entries only for functions with at least one free variable, while the
  -- latter maps the bound variables at any point in the program to their type.
  type LambdaLiftCaptureState = {
    -- A map from function identifier to a function producing a replacement
    -- expression for uses of the identifier. The arguments to the function is
    -- the type and info of the original variable. The replacement expression
    -- the captured variables applied to the original variable.
    --
    -- For example, if we have a function 'f' for which we are to capture free
    -- variables 'x' and 'y', then the identifier 'f' is mapped to 'f x y'.
    subs : Map Name (Map Name Type -> Type -> Info -> Expr),

    -- A map from function identifier to a pair of functions. The former
    -- updates the structure of its body by capturing its free variables, while
    -- the latter updates the type of its body accordingly.
    --
    -- Assume we have a function defined as 'let f : A -> B = lam x. e' and we
    -- are to capture the free variable 'y' of type 'C'. The first function
    -- will get 'lam x. e' as input, and its output is 'lam y. lam x. e', while
    -- the second gets 'A -> B' as input and outputs 'C -> A -> B'.
    lamIns : Map Name (Expr -> Expr, Type -> Type),

    -- Maps the identifiers of bound variables to the types they are given in
    -- the local scope.
    bound : Map Name Type
  }

  sem initLambdaLiftCaptureConfig : LambdaLiftSolutions -> LambdaLiftCaptureState
  sem initLambdaLiftCaptureConfig =
  | sols ->
    let subs =
      mapFoldWithKey
        (lam acc. lam id. lam solution.
          if mapIsEmpty solution then acc
          else
            let binds = mapBindings solution in
            let subExpr = lam bound. lam ty. lam info.
              -- NOTE(larshum, 2023-01-26): Compute the new type of the
              -- function, using variables bound in the local scope.
              let freeVars =
                map
                  (lam fv.
                    match fv with (fvId, _) in
                    match mapLookup fvId bound with Some fvTy then (fvId, fvTy)
                    else errorSingle [info] "Unbound variable found in lambda lifting")
                  binds
              in
              let ty =
                foldl
                  (lam ty. lam fv.
                    match fv with (_, fvTy) in
                    TyArrow {from = fvTy, to = ty, info = info})
                  ty freeVars
              in
              foldr
                (lam freeVar. lam acc.
                  match freeVar with (fvId, fvTy) in
                  let x = TmVar {ident = fvId, ty = fvTy, info = info,
                                 frozen = false} in
                  let ty =
                    match tyTm acc with TyArrow {from = fromTy, to = toTy} then
                      toTy
                    else
                      errorSingle [info] "Invalid type of application found in lambda lifting"
                  in
                  TmApp {lhs = acc, rhs = x, ty = ty, info = info})
                (TmVar {ident = id, ty = ty, info = info, frozen = false})
                (reverse freeVars)
            in
            mapInsert id subExpr acc)
        (mapEmpty nameCmp) sols
    in
    let lamIns =
      mapFoldWithKey
        (lam acc. lam id. lam solution.
          if mapIsEmpty solution then acc
          else
            let insertLambdas = lam body.
              let info = infoTm body in
              foldr
                (lam freeVar. lam acc.
                  match freeVar with (fvId, fvTy) in
                  let ty = TyArrow {from = fvTy, to = tyTm acc, info = info} in
                  TmLam {ident = fvId, tyAnnot = TyUnknown {info = info},
                         tyIdent = fvTy, body = acc, info = info, ty = ty})
                body (mapBindings solution)
            in
            recursive let updateTyBody = lam tyBody.
              match tyBody with TyAll t then
                TyAll {t with ty = updateTyBody t.ty}
              else
                let info = infoTy tyBody in
                foldr
                  (lam freeVar. lam acc.
                    match freeVar with (_, fvTy) in
                    TyArrow {from = fvTy, to = acc, info = info})
                  tyBody (mapBindings solution)
            in
            mapInsert id (insertLambdas, updateTyBody) acc)
        (mapEmpty nameCmp) sols
    in
    {subs = subs, lamIns = lamIns, bound = mapEmpty nameCmp}

  sem captureFreeVariablesH : LambdaLiftCaptureState -> Expr -> Expr
  sem captureFreeVariablesH state =
  | TmVar t ->
    match mapLookup t.ident state.subs with Some subFn then subFn state.bound t.ty t.info
    else TmVar t
  | TmLam t ->
    let state = {state with bound = mapInsert t.ident t.tyIdent state.bound} in
    TmLam {t with body = captureFreeVariablesH state t.body}
  | TmLet t ->
    let t = {t with body = captureFreeVariablesH state t.body} in
    let t =
      match mapLookup t.ident state.lamIns with Some (insertLambdas, updateTyBody) then
        let tyAnnot = TyUnknown {info = infoTy t.tyAnnot} in
        let body = insertLambdas t.body in
        let tyBody = updateTyBody t.tyBody in
        {t with tyAnnot = tyAnnot, tyBody = tyBody, body = body}
      else t
    in
    let state = {state with bound = mapInsert t.ident t.tyBody state.bound} in
    TmLet {t with inexpr = captureFreeVariablesH state t.inexpr}
  | TmRecLets t ->
    -- NOTE(larshum, 2023-01-26): First, we update the body and types of the
    -- bindings indepedently of each other. This is fine, as we already solved
    -- the set equations to determine which free variables need to be captured.
    let bindings =
      map
        (lam bind.
          match mapLookup bind.ident state.lamIns with Some (insertLambdas, updateTyBody) then
            let tyAnnot = TyUnknown {info = infoTy bind.tyAnnot} in
            {bind with tyAnnot = TyUnknown {info = infoTy bind.tyAnnot},
                       tyBody = updateTyBody bind.tyBody,
                       body = insertLambdas bind.body}
          else bind)
        t.bindings
    in

    -- NOTE(larshum, 2023-01-26): Insert the updated types of the bindings into
    -- the state of bound variables.
    let state =
      foldl
        (lam state. lam bind.
          {state with bound = mapInsert bind.ident bind.tyBody state.bound})
        state bindings
    in

    -- NOTE(larshum, 2023-01-26): Finally, we insert captured variables in the
    -- bodies of the bindings.
    let bindings =
      map
        (lam bind.
          {bind with body = captureFreeVariablesH state bind.body})
        bindings
    in
    TmRecLets {t with bindings = bindings,
                      inexpr = captureFreeVariablesH state t.inexpr}
  | TmMatch t ->
    let thnState = {state with bound = boundVariablesInPattern state.bound t.pat} in
    TmMatch {t with target = captureFreeVariablesH state t.target,
                    thn = captureFreeVariablesH thnState t.thn,
                    els = captureFreeVariablesH state t.els}
  | t -> smap_Expr_Expr (captureFreeVariablesH state) t

  sem captureFreeVariables : LambdaLiftSolutions -> Expr -> Expr
  sem captureFreeVariables sols =
  | t ->
    let config = initLambdaLiftCaptureConfig sols in
    captureFreeVariablesH config t
end

-- Lifts nested function definitions to the top of the program. As nested
-- functions may be dependent on local definitions of types, constructors, and
-- external functions, these must also be lifted to the global scope.
lang LambdaLiftGlobal = LambdaLift
  sem liftGlobal : Expr -> Expr
  sem liftGlobal =
  | TmLet t ->
    match liftBody [] t.body with (lifted, body) in
    bindLifted
      (TmLet {t with body = body, inexpr = liftGlobal t.inexpr})
      lifted
  | TmRecLets t ->
    let liftBindingBody = lam lifted. lam bind.
      match liftBody lifted bind.body with (lifted, body) in
      (lifted, {bind with body = body})
    in
    match mapAccumL liftBindingBody [] t.bindings with (lifted, bindings) in
    match bindRecursive ([], bindings) lifted with (lifted, bindings) in
    bindLifted
      (TmRecLets {t with bindings = bindings, inexpr = liftGlobal t.inexpr})
      lifted
  | TmType t -> TmType {t with inexpr = liftGlobal t.inexpr}
  | TmConDef t -> TmConDef {t with inexpr = liftGlobal t.inexpr}
  | TmExt t -> TmExt {t with inexpr = liftGlobal t.inexpr}
  | TmUtest t ->
    match liftBody [] t.test with (lifted, test) in
    match liftBody lifted t.expected with (lifted, expected) in
    bindLifted
      (TmUtest {t with test = test, expected = expected,
                       next = liftGlobal t.next})
      lifted
  | t ->
    match liftBody [] t with (lifted, t) in
    bindLifted t lifted

  sem liftBody : [Expr] -> Expr -> ([Expr], Expr)
  sem liftBody acc =
  | TmLet t ->
    match liftBody acc t.body with (acc, body) in
    match liftBody acc t.inexpr with (acc, inexpr) in
    if isFunctionType t.tyBody then
      (snoc acc (TmLet {t with body = body, inexpr = unit_}), inexpr)
    else
      (acc, TmLet {t with body = body, inexpr = inexpr})
  | TmRecLets t ->
    let liftBindingBody = lam lifted. lam bind.
      match liftBody lifted bind.body with (lifted, body) in
      (lifted, {bind with body = body})
    in
    match mapAccumL liftBindingBody [] t.bindings with (lifted, bindings) in
    match bindRecursive ([], bindings) lifted with (lifted, bindings) in
    match liftBody acc t.inexpr with (acc, inexpr) in
    (snoc acc (TmRecLets {t with bindings = bindings, inexpr = unit_}), inexpr)
  | TmType t ->
    match liftBody acc t.inexpr with (acc, inexpr) in
    (snoc acc (TmType {t with inexpr = unit_}), inexpr)
  | TmConDef t ->
    match liftBody acc t.inexpr with (acc, inexpr) in
    (snoc acc (TmConDef {t with inexpr = unit_}), inexpr)
  | TmExt t ->
    match liftBody acc t.inexpr with (acc, inexpr) in
    (snoc acc (TmExt {t with inexpr = unit_}), inexpr)
  | t -> smapAccumL_Expr_Expr liftBody acc t

  sem bindLifted : Expr -> [Expr] -> Expr
  sem bindLifted ast =
  | [h] ++ t ->
    let inexpr = bindLifted ast t in
    let ty = tyTm inexpr in
    switch h
    case TmLet t then TmLet {t with inexpr = inexpr, ty = ty}
    case TmRecLets t then TmRecLets {t with inexpr = inexpr, ty = ty}
    case TmType t then TmType {t with inexpr = inexpr, ty = ty}
    case TmConDef t then TmConDef {t with inexpr = inexpr, ty = ty}
    case TmExt t then TmExt {t with inexpr = inexpr, ty = ty}
    end
  | [] -> ast

  sem bindRecursive : ([Expr], [RecLetBinding]) -> [Expr]
                   -> ([Expr], [RecLetBinding])
  sem bindRecursive acc =
  | [TmLet t] ++ rest ->
    match bindRecursive acc rest with (lifted, bindings) in
    let bind = {
      ident = t.ident, tyAnnot = t.tyAnnot, tyBody = t.tyBody, body = t.body,
      info = t.info
    } in
    (lifted, cons bind bindings)
  | [TmRecLets t] ++ rest ->
    match bindRecursive acc rest with (lifted, bindings) in
    (lifted, concat t.bindings bindings)
  | [h & (TmType _ | TmConDef _ | TmExt _)] ++ rest ->
    match bindRecursive acc rest with (lifted, bindings) in
    (snoc lifted h, bindings)
  | [] -> acc
end

-- Resymbolizes all identifiers of variables that were captured during lambda
-- lifting. This ensures that identifiers representing distinct variables in
-- the program are globally unique, which is a key property other
-- transformations may depend on.
lang LambdaLiftRenameCapturedVariables = LambdaLift + MExprSubstitute
  -- NOTE(larshum, 2023-01-27): We run the renaming of captured variables after
  -- lifting nested functions to the global scope. Therefore, we do not need to
  -- recurse into the bodies of the let- and recursive let-bindings.
  sem renameCapturedVariablesH : Map Name (Map Name Name) -> Expr -> Expr
  sem renameCapturedVariablesH subMap =
  | TmLet t ->
    let body =
      match mapLookup t.ident subMap with Some subs then
        substituteIdentifiers subs t.body
      else t.body
    in
    TmLet {t with body = body,
                  inexpr = renameCapturedVariablesH subMap t.inexpr}
  | TmRecLets t ->
    let renameBinding = lam bind.
      match mapLookup bind.ident subMap with Some subs then
        if mapIsEmpty subs then {bind with body = bind.body}
        else {bind with body = substituteIdentifiers subs bind.body}
      else
        errorSingle [bind.info] "Lambda lifting missing solution for recursive binding"
    in
    TmRecLets {t with bindings = map renameBinding t.bindings,
                      inexpr = renameCapturedVariablesH subMap t.inexpr}
  | t -> smap_Expr_Expr (renameCapturedVariablesH subMap) t

  sem renameCapturedVariables : LambdaLiftSolutions -> Expr
                             -> (LambdaLiftSolutions, Expr)
  sem renameCapturedVariables solutions =
  | t ->
    let subs : Map Name [(Name, Name, Type)] =
      mapMapWithKey
        (lam id. lam fv.
          map
            (lam v.
              match v with (fvId, fvTy) in
              (fvId, nameSetNewSym fvId, fvTy))
            (mapBindings fv))
        solutions
    in

    -- NOTE(larshum, 2023-01-27): Construct a substitution map, which maps the
    -- original identifier of the captured free variable to an identifier with
    -- a distinct symbol.
    let nameSub = lam sub.
      match sub with (oldId, newId, _) in
      (oldId, newId)
    in
    let subMap : Map Name (Map Name Name) =
      mapMapWithKey
        (lam. lam subs. mapFromSeq nameCmp (map nameSub subs))
        subs
    in

    -- NOTE(larshum, 2023-01-27): Reconstruct the solutions using the
    -- identifiers after renaming captured variables.
    let useNewId = lam sub.
      match sub with (_, newId, ty) in
      (newId, ty)
    in
    let solutions : LambdaLiftSolutions =
      mapMapWithKey
        (lam. lam subs. mapFromSeq nameCmp (map useNewId subs))
        subs
    in

    (solutions, renameCapturedVariablesH subMap t)
end

lang MExprLambdaLift =
  LambdaLiftNameAnonymous + LambdaLiftTypeVariables +
  LambdaLiftSolveEquations + LambdaLiftCaptureVariables +
  LambdaLiftGlobal + LambdaLiftRenameCapturedVariables

  sem liftLambdas : Expr -> Expr
  sem liftLambdas =
  | t -> match liftLambdasWithSolutions t with (_, t) in t

  sem liftLambdasWithSolutions : Expr -> (LambdaLiftSolutions, Expr)
  sem liftLambdasWithSolutions =
  | ast ->
    -- Binds all anonymous functions in the AST to new, globally unique,
    -- identifiers.
    let ast = nameAnonymousFunctions ast in

    -- Captures free type variables by introducing forall quantifiers inside
    -- nested functions that refer to type variables introduced outside of
    -- them.
    let ast = captureTypeVariables ast in

    -- Solves the set equations for all functions in the AST, by finding the
    -- free variables that need to be captured in order to make all functions
    -- closed. The resulting solution (contained in state.sols) maps the
    -- identifier of each function to a pair of identifiers and types
    -- corresponding to each free variable that needs to be captured to make
    -- the function closed.
    let state = solveSetEquations (lambdaLiftStateEmpty ()) ast in

    -- Captures the free variables in all functions of the AST, according to
    -- the solutions found in the previous step. After this step, all functions
    -- are closed and can safely be lifted to the global scope.
    let ast = captureFreeVariables state.sols ast in

    -- Lifts all nested function definitions to the top of the program. Note
    -- that nested bindings within a recursive let-binding are lifted out as
    -- bindings of the recursive let-expression.
    let ast = liftGlobal ast in

    -- Resymbolizes the captured variables in all functions to restore the
    -- property that all variables in the program have a globally distinct
    -- identifier. This happens because the 'captureFreeVariables' function
    -- uses the name of the captured variable, resulting in multiple functions
    -- using the same name.
    renameCapturedVariables state.sols ast
end

lang TestLang =
  MExprLambdaLift + MExprSym + MExprTypeCheck + MExprEq + MExprPrettyPrint
end

mexpr

use TestLang in

let preprocess = lam ast. typeCheck (symbolize ast) in

----------------------------------------------------------------
-- Tests that anonymous functions are bound to an identifier. --
----------------------------------------------------------------

let anonLambda = preprocess (ulam_ "x" (var_ "x")) in
let expected = preprocess (bind_ (ulet_ "t" (ulam_ "x" (var_ "x"))) (var_ "t")) in
utest nameAnonymousFunctions anonLambda with expected using eqExpr in

let multipleLambda = preprocess (ulam_ "x" (ulam_ "y" (addi_ (var_ "x") (var_ "y")))) in
utest nameAnonymousFunctions multipleLambda
with preprocess (bind_ (ulet_ "t" multipleLambda) (var_ "t"))
using eqExpr in

let anonLambdaInLet = preprocess (ulet_ "f" (ulam_ "x" (ulam_ "y" (bind_
    (ulet_ "z" (int_ 4))
    (ulam_ "y" (addi_ (var_ "x") (var_ "y"))))
))) in
let expected = preprocess (ulet_ "f" (ulam_ "x" (ulam_ "y" (bindall_ [
    ulet_ "z" (int_ 4),
    ulet_ "t" (ulam_ "y" (addi_ (var_ "x") (var_ "y"))),
    var_ "t"
])))) in
utest nameAnonymousFunctions anonLambdaInLet with expected using eqExpr in

let anonLambdaInReclets = preprocess (ureclets_ [
  ("f", ulam_ "n"
    (if_ (eqi_ (var_ "n") (int_ 0))
      (app_ (ulam_ "x" (var_ "x")) (int_ 1))
      (muli_ (var_ "n") (app_ (var_ "f") (subi_ (var_ "n") (int_ 1))))))
]) in
let expected = preprocess (ureclets_ [
  ("f", ulam_ "n"
    (if_ (eqi_ (var_ "n") (int_ 0))
      (app_ (bind_ (ulet_ "t" (ulam_ "x" (var_ "x"))) (var_ "t")) (int_ 1))
      (muli_ (var_ "n") (app_ (var_ "f") (subi_ (var_ "n") (int_ 1))))))
]) in
utest nameAnonymousFunctions anonLambdaInReclets with expected using eqExpr in

let nestedAnon = preprocess (ulam_ "x" (bindall_ [
  ulet_ "y" (ulam_ "y" (addi_ (var_ "y") (int_ 1))),
  ulam_ "z" (addi_ (app_ (var_ "y") (var_ "x")) (var_ "z"))
])) in
let expected = preprocess (bind_ (ulet_ "t" (ulam_ "x" (bindall_ [
    ulet_ "y" (ulam_ "y" (addi_ (var_ "y") (int_ 1))),
    ulet_ "t" (ulam_ "z" (addi_ (app_ (var_ "y") (var_ "x")) (var_ "z"))),
    var_ "t"
  ]))) (var_ "t")
) in
utest nameAnonymousFunctions nestedAnon with expected using eqExpr in

-----------------------------------------------------
-- Tests that type variables are properly captured --
-----------------------------------------------------

let typeOf = lam id. lam e.
  recursive let work : Name -> Option Type -> Expr -> Option Type =
    lam id. lam acc. lam e.
    match acc with Some _ then acc else
    switch e
    case TmLet t then
      if nameEq id t.ident then Some t.tyBody
      else
        match work id acc t.body with Some x then
          Some x
        else work id acc t.inexpr
    case TmRecLets t then
      let workBind = lam acc. lam bind.
        match acc with Some _ then acc
        else if nameEq id bind.ident then Some bind.tyBody
        else work id acc bind.body
      in
      let acc = foldl workBind acc t.bindings in
      work id acc t.inexpr
    case t then sfold_Expr_Expr (work id) acc t
    end
  in
  match work id (None ()) e with Some ty then ty
  else error "Type not found"
in

let x = nameSym "x" in
let y = nameSym "y" in
let z = nameSym "z" in
let w = nameSym "w" in
let f = nameSym "f" in
let g = nameSym "g" in
let h = nameSym "h" in
let a = nameSym "a" in
let b = nameSym "b" in
let c = nameSym "c" in

-- Tests that both types are of the shape 'all x. <ty>' where the type variable
-- 'x' is distinct for the two types.
let distinctTypeVariableIdentifier : Type -> Type -> Bool =
  lam ty1. lam ty2.
  switch (ty1, ty2)
  case (TyAll l, TyAll r) then not (nameEq l.ident r.ident)
  case _ then false
  end
in

let ty1 = tyint_ in
let ty2 = tyall_ "a" (tyvar_ "a") in
let ty3 = tyall_ "b" (tyvar_ "b") in
let ty4 = ntyall_ a (ntyvar_ a) in
utest distinctTypeVariableIdentifier ty1 ty2 with false in
utest distinctTypeVariableIdentifier ty2 ty3 with true in
utest distinctTypeVariableIdentifier ty2 ty4 with true in
utest distinctTypeVariableIdentifier ty4 ty4 with false in

let freeInnerTyVar = preprocess (bindall_ [
  nlet_ f (ntyall_ a (tyarrows_ [ntyvar_ a, ntyvar_ a])) (nulam_ x (bindall_ [
    nlet_ g (tyarrow_ (tyrecord_ []) (ntyvar_ a)) (ulam_ "" (nvar_ x)),
    app_ (nvar_ g) (urecord_ [])
  ])),
  utuple_ [app_ (nvar_ f) (int_ 1), app_ (nvar_ f) true_]
]) in
let result = captureTypeVariables freeInnerTyVar in
let fty = typeOf f result in
let gty = typeOf g result in
utest fty with ntyall_ a (tyarrow_ (ntyvar_ a) (ntyvar_ a)) using eqType in
utest gty with ntyall_ a (tyarrow_ (tyrecord_ []) (ntyvar_ a)) using eqType in
utest fty with gty using distinctTypeVariableIdentifier in

let fty = ntyall_ a (tyarrows_ [tyarrow_ (ntyvar_ a) (ntyvar_ a), ntyvar_ a, ntyvar_ a]) in
let innerRecLets = preprocess (bindall_ [
  nlet_ f fty (nulam_ g (nulam_ x (bindall_ [
    nreclets_ [
      (h, tyarrow_ (tyrecord_ []) (ntyvar_ a), ulam_ "" (app_ (nvar_ g) (nvar_ x)))
    ],
    app_ (nvar_ h) (urecord_ [])
  ]))),
  utuple_ [
    appf2_ (nvar_ f) (ulam_ "x" (addi_ (var_ "x") (int_ 1))) (int_ 2),
    appf2_ (nvar_ f) (ulam_ "x" (var_ "x")) (char_ 'x')
  ]
]) in
let result = captureTypeVariables innerRecLets in
let expectedType = ntyall_ a (tyarrows_ [tyrecord_ [], ntyvar_ a]) in
let ft = typeOf f result in
let ht = typeOf h result in
utest ft with fty using eqType in
utest ht with expectedType using eqType in
utest ft with ht using distinctTypeVariableIdentifier in

----------------------------------------------------------------------------
-- Tests that the solutions produced by solving the set equations contain --
-- the expected set of free variables with the correct types.             --
----------------------------------------------------------------------------

let solveSetEquationsDefault = solveSetEquations (lambdaLiftStateEmpty ()) in

let capturedVar = preprocess (bindall_ [
  nulet_ x (int_ 2),
  nulet_ f (nulam_ y (addi_ (nvar_ x) (nvar_ y)))
]) in
let state = solveSetEquationsDefault capturedVar in
utest state.vars with setOfSeq nameCmp [x, y] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapFromSeq nameCmp [(x, tyint_)])
] using mapEq (mapEq eqType) in

let matchExpr = preprocess (nureclets_ [
  (f, nulam_ a (match_
    (nvar_ a) (pseqedgen_ [npvar_ x] y [])
    (app_ (nvar_ f) (nvar_ y))
    (seq_ [])))
]) in
let state = solveSetEquationsDefault matchExpr in
utest state.vars with setOfSeq nameCmp [a, x, y] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapEmpty nameCmp)
] using mapEq (mapEq eqType) in

let sequentialDependencies = preprocess (bindall_ [
  nulet_ x (int_ 4),
  nulet_ f (nulam_ y (addi_ (nvar_ y) (nvar_ x))),
  nulet_ g (nulam_ z (app_ (nvar_ f) (nvar_ z)))
]) in
let state = solveSetEquationsDefault sequentialDependencies in
utest state.vars with setOfSeq nameCmp [x, y, z] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapFromSeq nameCmp [(x, tyint_)]),
  (g, mapFromSeq nameCmp [(x, tyint_)])
] using mapEq (mapEq eqType) in

let recBinds = preprocess (bindall_ [
  nulet_ w (int_ 3),
  nureclets_ [
    (f, nulam_ x (addi_ (nvar_ x) (nvar_ w))),
    (g, nulam_ y (app_ (nvar_ f) (nvar_ y))),
    (h, nulam_ z (app_ (nvar_ g) (nvar_ z)))
  ]
]) in
let state = solveSetEquationsDefault recBinds in
utest state.vars with setOfSeq nameCmp [x, y, z, w] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapFromSeq nameCmp [(w, tyint_)]),
  (g, mapFromSeq nameCmp [(w, tyint_)]),
  (h, mapFromSeq nameCmp [(w, tyint_)])
] using mapEq (mapEq eqType) in

let recSystem = preprocess (bindall_ [
  nulet_ a (int_ 1),
  nulet_ b (int_ 2),
  nulet_ c (int_ 3),
  nureclets_ [
    (f, nulam_ x (addi_ (app_ (nvar_ g) (nvar_ x)) (nvar_ a))),
    (g, nulam_ y (addi_ (app_ (nvar_ f) (nvar_ y)) (nvar_ b))),
    (h, nulam_ z (addi_ (nvar_ z) (nvar_ c)))
  ]
]) in
let state = solveSetEquationsDefault recSystem in
utest state.vars with setOfSeq nameCmp [x, y, z, a, b, c] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapFromSeq nameCmp [(a, tyint_), (b, tyint_)]),
  (g, mapFromSeq nameCmp [(a, tyint_), (b, tyint_)]),
  (h, mapFromSeq nameCmp [(c, tyint_)])
] using mapEq (mapEq eqType) in

let boundInPat = preprocess (
  nulet_ f (nulam_ x (match_ (nvar_ x) (npvar_ y) (nvar_ y) never_))
) in
let state = solveSetEquationsDefault boundInPat in
utest state.vars with setOfSeq nameCmp [x, y] using setEq in
utest state.sols with mapFromSeq nameCmp [(f, mapEmpty nameCmp)]
using mapEq (mapEq eqType) in

let nestedBinds = preprocess (bindall_ [
  nulet_ z (int_ 3),
  nulet_ f (nulam_ x (bindall_ [
    nulet_ g (nulam_ y (addi_ (nvar_ x) (subi_ (nvar_ y) (nvar_ z)))),
    app_ (nvar_ g) (nvar_ x)
  ]))
]) in
let state = solveSetEquationsDefault nestedBinds in
utest state.vars with setOfSeq nameCmp [x, y, z] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapFromSeq nameCmp [(z, tyint_)]),
  (g, mapFromSeq nameCmp [(x, tyint_), (z, tyint_)])
] using mapEq (mapEq eqType) in

let gty = tyarrow_ (tyrecord_ []) tyint_ in
let higherOrderFun = preprocess (bindall_ [
  nulet_ f (nlam_ g gty (bindall_ [
    nulet_ h (nulam_ y (app_ (nvar_ g) uunit_)),
    app_ (nvar_ h) (urecord_ [])
  ])),
  nulet_ x (int_ 4),
  addi_
    (app_ (nvar_ f) (nulam_ z (nvar_ x)))
    (int_ 1)
]) in
let state = solveSetEquationsDefault higherOrderFun in
utest state.vars with setOfSeq nameCmp [g, x, y, z] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapEmpty nameCmp),
  (h, mapFromSeq nameCmp [(g, gty)])
] using mapEq (mapEq eqType) in

----------------------------------------------------------------------
-- Tests that free variables are correctly captured in the program, --
-- according to the provided solutions.                             --
----------------------------------------------------------------------

let capturedVar = preprocess (bindall_ [
  nulet_ x (int_ 2),
  nulet_ f (nulam_ y (addi_ (nvar_ x) (nvar_ y)))
]) in
let state = solveSetEquationsDefault capturedVar in
let expected = preprocess (bindall_ [
  nulet_ x (int_ 2),
  nulet_ f (nulam_ x (nulam_ y (addi_ (nvar_ x) (nvar_ y))))
]) in
utest captureFreeVariables state.sols capturedVar with expected using eqExpr in

let nestedBinds = preprocess (bindall_ [
  nulet_ z (int_ 3),
  nulet_ f (nulam_ x (bindall_ [
    nulet_ g (nulam_ y (addi_ (nvar_ x) (subi_ (nvar_ y) (nvar_ z)))),
    app_ (nvar_ g) (nvar_ x)
  ]))
]) in
let state = solveSetEquationsDefault nestedBinds in
let expected = preprocess (bindall_ [
  nulet_ z (int_ 3),
  nulet_ f (nulam_ z (nulam_ x (bindall_ [
    nulet_ g (nulam_ x (nulam_ z (nulam_ y (addi_ (nvar_ x) (subi_ (nvar_ y) (nvar_ z)))))),
    appf3_ (nvar_ g) (nvar_ x) (nvar_ z) (nvar_ x)
  ])))
]) in
utest captureFreeVariables state.sols nestedBinds with expected using eqExpr in

---------------------------------------------------------------------------
-- Tests that nested functions are correctly lifted to the global scope. --
---------------------------------------------------------------------------

let fun = preprocess (ulet_ "f" (ulam_ "x" (ulam_ "y" (bindall_ [
  ulet_ "z" (addi_ (var_ "x") (var_ "y")),
  ulet_ "w" (subi_ (var_ "x") (var_ "y")),
  divi_ (var_ "z") (var_ "w")
])))) in
utest liftGlobal fun with fun using eqExpr in

let nestedLets = preprocess (ulet_ "f" (ulam_ "x" (bindall_ [
  ulet_ "g" (ulam_ "y" (addi_ (var_ "y") (int_ 1))),
  addi_ (var_ "x") (app_ (var_ "g") (var_ "x"))
]))) in
let expected = preprocess (bindall_ [
  ulet_ "g" (ulam_ "y" (addi_ (var_ "y") (int_ 1))),
  ulet_ "f" (ulam_ "x" (addi_ (var_ "x") (app_ (var_ "g") (var_ "x")))),
  unit_
]) in
utest liftGlobal nestedLets with expected using eqExpr in

let nestedBindings = preprocess (ulet_ "f" (ulam_ "x" (bindall_ [
  ureclets_ [("g", ulam_ "y" (
    bind_
      (ulet_ "h" (ulam_ "z" (addi_ (var_ "z") (var_ "z"))))
      (app_ (var_ "h") (int_ 1))
  ))],
  app_ (var_ "g") (var_ "x")
]))) in
let expected = preprocess (bindall_ [
  ureclets_ [
    ("h", ulam_ "z" (addi_ (var_ "z") (var_ "z"))),
    ("g", ulam_ "y" (app_ (var_ "h") (int_ 1)))
  ],
  ulet_ "f" (ulam_ "x" (app_ (var_ "g") (var_ "x"))),
  unit_
]) in
utest liftGlobal nestedBindings with expected using eqExpr in

let nestedReclets = preprocess (ureclets_ [
  ("f", ulam_ "x" (ureclets_ [
    ("g", (ulam_ "y" (ureclets_ [
      ("h", (ulam_ "z" (var_ "z")))
    ])))
  ]))
]) in
let expected = preprocess (ureclets_ [
  ("h", ulam_ "z" (var_ "z")),
  ("g", ulam_ "y" unit_),
  ("f", ulam_ "x" unit_)
]) in
utest liftGlobal nestedReclets with expected using eqExpr in

let innerTypeCon = preprocess (ureclets_ [
  ("f", ulam_ "x" (bindall_ [
    type_ "Tree" [] (tyvariant_ []),
    condef_ "Leaf" (tyarrow_ tyint_ (tycon_ "Tree")),
    condef_ "Branch" (tyarrow_ (tytuple_ [tycon_ "Tree", tycon_ "Tree"]) (tycon_ "Tree")),
    ulet_ "g" (ulam_ "x" (ulam_ "y" (
      conapp_ "Branch" (utuple_ [conapp_ "Leaf" (var_ "x"), conapp_ "Leaf" (var_ "y")])
    ))),
    appf2_ (var_ "g") (var_ "x") (int_ 0)
  ]))
]) in
let expected = preprocess (bindall_ [
  type_ "Tree" [] (tyvariant_ []),
  condef_ "Leaf" (tyarrow_ tyint_ (tycon_ "Tree")),
  condef_ "Branch" (tyarrow_ (tytuple_ [tycon_ "Tree", tycon_ "Tree"]) (tycon_ "Tree")),
  ureclets_ [
    ("g", ulam_ "x" (ulam_ "y"
      (conapp_
        "Branch"
        (utuple_ [conapp_ "Leaf" (var_ "x"), conapp_ "Leaf" (var_ "y")])))),
    ("f", ulam_ "x" (appf2_ (var_ "g") (var_ "x") (int_ 0)))
  ]
]) in
-- NOTE(larshum, 2023-01-27): We cannot compare TmType using eqExpr, so we
-- pretty-print both ASTs and test that they are equal.
utest expr2str (liftGlobal innerTypeCon) with expr2str expected using eqString in

---------------------------------------------------------------------
-- Complete lambda lifting tests involving all steps tested above. --
---------------------------------------------------------------------

let anonInSeq = preprocess (nulam_ x (seq_ [
  nulam_ y (addi_ (nvar_ x) (nvar_ y)),
  nulam_ z (addi_ (nvar_ x) (nvar_ z))
])) in
let expected = preprocess (bind_ (ulet_ "a" (nulam_ x (seq_ [
  bind_ (ulet_ "b" (nulam_ y (addi_ (nvar_ x) (nvar_ y)))) (var_ "b"),
  bind_ (ulet_ "c" (nulam_ z (addi_ (nvar_ x) (nvar_ z)))) (var_ "c")
]))) (var_ "a")) in
let ast = nameAnonymousFunctions anonInSeq in
utest ast with expected using eqExpr in
let state = solveSetEquationsDefault ast in
utest state.vars with setOfSeq nameCmp [x, y, z] using setEq in
utest mapSize state.sols with 3 in
let expected = preprocess (bindall_ [
  ulet_ "b" (nulam_ x (nulam_ y (addi_ (nvar_ x) (nvar_ y)))),
  ulet_ "c" (nulam_ x (nulam_ z (addi_ (nvar_ x) (nvar_ z)))),
  ulet_ "a" (nulam_ x (seq_ [ app_ (var_ "b") (nvar_ x)
                            , app_ (var_ "c") (nvar_ x) ])),
  var_ "a"
]) in
utest liftLambdas anonInSeq with expected using eqExpr in

let nolam = ulet_ "x" (int_ 2) in
utest liftLambdas nolam with nolam using eqExpr in

let innerFun = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x"
    (bind_
      (ulet_ "g" (ulam_ "y" (addi_ (var_ "y") (int_ 2))))
      (muli_ (app_ (var_ "g") (var_ "x")) (int_ 2)))),
  app_ (var_ "f") (int_ 1)
]) in
let expected = preprocess (bindall_ [
  ulet_ "g" (ulam_ "y" (addi_ (var_ "y") (int_ 2))),
  ulet_ "f" (ulam_ "x" (muli_ (app_ (var_ "g") (var_ "x")) (int_ 2))),
  app_ (var_ "f") (int_ 1)
]) in
utest liftLambdas innerFun with expected using eqExpr in

let factorial = preprocess (ureclets_ [
  ("fact", ulam_ "n"
    (if_ (eqi_ (var_ "n") (int_ 0))
      (int_ 1)
      (muli_ (var_ "n") (app_ (var_ "fact") (subi_ (var_ "n") (int_ 1))))))
]) in
utest liftLambdas factorial with factorial using eqExpr in

let factorialWithHelper = preprocess (bindall_ [
  ulet_ "fact" (ulam_ "n" (bindall_ [
    ureclets_ [
      ("work", ulam_ "acc" (ulam_ "n" (
        if_ (eqi_ (var_ "n") (int_ 0))
          (var_ "acc")
          (appf2_ (var_ "work")
            (muli_ (var_ "acc") (var_ "n"))
            (subi_ (var_ "n") (int_ 1))))))],
    appf2_ (var_ "work") (int_ 1) (var_ "n")])),
  app_ (var_ "fact") (int_ 4)]) in
let expected = preprocess (bindall_ [
  ureclets_ [
    ("work", ulam_ "acc" (ulam_ "n" (
      if_ (eqi_ (var_ "n") (int_ 0))
        (var_ "acc")
        (appf2_ (var_ "work")
          (muli_ (var_ "acc") (var_ "n"))
          (subi_ (var_ "n") (int_ 1))))))],
  ulet_ "fact" (ulam_ "n" (appf2_ (var_ "work") (int_ 1) (var_ "n"))),
  app_ (var_ "fact") (int_ 4)]) in
utest liftLambdas factorialWithHelper with expected using eqExpr in

let liftFreeVars = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (bindall_ [
    ulet_ "g" (ulam_ "y" (addi_ (var_ "x") (var_ "y"))),
    app_ (var_ "g") (int_ 2)])),
  app_ (var_ "f") (int_ 3)]) in
let expected = preprocess (bindall_ [
  ulet_ "g" (ulam_ "x" (ulam_ "y" (addi_ (var_ "x") (var_ "y")))),
  ulet_ "f" (ulam_ "x" (appf2_ (var_ "g") (var_ "x") (int_ 2))),
  app_ (var_ "f") (int_ 3)]) in
utest liftLambdas liftFreeVars with expected using eqExpr in

let deepNesting = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (bindall_ [
    ulet_ "g" (ulam_ "y" (bindall_ [
      ulet_ "h" (ulam_ "z" (
        addi_ (var_ "y") (var_ "z"))),
      app_ (var_ "h") (int_ 2)])),
    app_ (var_ "g") (var_ "x")])),
  app_ (var_ "f") (int_ 1)]) in
let expected = preprocess (bindall_ [
  ulet_ "h" (ulam_ "y" (ulam_ "z" (addi_ (var_ "y") (var_ "z")))),
  ulet_ "g" (ulam_ "y" (appf2_ (var_ "h") (var_ "y") (int_ 2))),
  ulet_ "f" (ulam_ "x" (app_ (var_ "g") (var_ "x"))),
  app_ (var_ "f") (int_ 1)]) in
utest liftLambdas deepNesting with expected using eqExpr in

let multipleInnerLets = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (bindall_ [
    ulet_ "g" (ulam_ "y" (addi_ (var_ "x") (var_ "y"))),
    ulet_ "h" (ulam_ "z" (addi_ (var_ "z") (var_ "x"))),
    addi_ (app_ (var_ "g") (int_ 1)) (app_ (var_ "h") (int_ 2))])),
  app_ (var_ "f") (int_ 1)]) in
let expected = preprocess (bindall_ [
  ulet_ "h" (ulam_ "x" (ulam_ "z" (addi_ (var_ "z") (var_ "x")))),
  ulet_ "g" (ulam_ "x" (ulam_ "y" (addi_ (var_ "x") (var_ "y")))),
  ulet_ "f" (ulam_ "x" (
    addi_ (appf2_ (var_ "g") (var_ "x") (int_ 1))
          (appf2_ (var_ "h") (var_ "x") (int_ 2)))),
  app_ (var_ "f") (int_ 1)]) in
utest liftLambdas multipleInnerLets with expected using eqExpr in

let letInReclet = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (bindall_ [
    ureclets_ [
      ("g", ulam_ "y" (bindall_ [
        ulet_ "h" (ulam_ "z" (addi_ (var_ "z") (int_ 1))),
        addi_ (app_ (var_ "h") (var_ "y")) (var_ "x")
      ]))],
    app_ (var_ "g") (int_ 1)
  ])),
  app_ (var_ "f") (int_ 4)]) in
let expected = preprocess (bindall_ [
  ureclets_ [
    ("h", ulam_ "z" (addi_ (var_ "z") (int_ 1))),
    ("g", ulam_ "x" (ulam_ "y" (
      addi_ (app_ (var_ "h") (var_ "y")) (var_ "x"))))],
  ulet_ "f" (ulam_ "x" (appf2_ (var_ "g") (var_ "x") (int_ 1))),
  app_ (var_ "f") (int_ 4)]) in
utest liftLambdas letInReclet with expected using eqExpr in

let deepNestedRecursiveDefs = preprocess (ureclets_ [
  ("f1", ulam_ "x" (bindall_ [
    ulet_ "f2" (bindall_ [
      ureclets_ [("f3", ulam_ "x1" (addi_ (var_ "x1") (int_ 1)))],
      ureclets_ [
        ("f4", ulam_ "y" (bindall_ [
          ulet_ "k" (ulam_ "x2" (app_ (var_ "f5") (var_ "x2"))),
          addi_ (app_ (var_ "k") (var_ "x")) (var_ "y")])),
        ("f5", ulam_ "x3" (app_ (var_ "f4") (subi_ (var_ "x3") (int_ 1))))
      ],
      addi_ (app_ (var_ "f3") (var_ "x"))
            (app_ (var_ "f4") (int_ 2))]),
    var_ "f2"]))]) in
let expected = preprocess (ureclets_ [
  ("k", ulam_ "x" (ulam_ "x2" (appf2_ (var_ "f5") (var_ "x") (var_ "x2")))),
  ("f4", ulam_ "x" (ulam_ "y" (addi_ (appf2_ (var_ "k") (var_ "x") (var_ "x")) (var_ "y")))),
  ("f5", ulam_ "x" (ulam_ "x3" (appf2_ (var_ "f4") (var_ "x") (subi_ (var_ "x3") (int_ 1))))),
  ("f3", ulam_ "x1" (addi_ (var_ "x1") (int_ 1))),
  ("f1", ulam_ "x" (bindall_ [
    ulet_ "f2" (addi_ (app_ (var_ "f3") (var_ "x"))
                      (appf2_ (var_ "f4") (var_ "x") (int_ 2))),
    var_ "f2"]))]) in
utest liftLambdas deepNestedRecursiveDefs with expected using eqExpr in

let fdef = ulet_ "f" (ulam_ "x" (addi_ (var_ "x") (int_ 1))) in
let fapp = bind_ fdef (app_ (var_ "f") (int_ 1)) in

let liftUtest = preprocess (
  utest_
    (int_ 0)
    fapp
    unit_) in
let expected = preprocess (
  bind_
    fdef
    (utest_
      (int_ 0)
      (app_ (var_ "f") (int_ 1))
      unit_)) in
utest liftLambdas liftUtest with expected using eqExpr in

let liftApp = preprocess (bindall_ [
  app_
    (bind_
      (ulet_ "g" (ulam_ "x" (ulam_ "y" (addi_ (var_ "x") (var_ "y")))))
      (app_ (var_ "g") (int_ 2)))
    fapp]) in
let expected = preprocess (bindall_ [
  ulet_ "g" (ulam_ "x" (ulam_ "y" (addi_ (var_ "x") (var_ "y")))),
  fdef,
  app_
    (app_ (var_ "g") (int_ 2))
    (app_ (var_ "f") (int_ 1))]) in
utest liftLambdas liftApp with expected using eqExpr in

let liftSeq = preprocess (seq_ [fapp]) in
let expected = preprocess (bindall_ [fdef, seq_ [app_ (var_ "f") (int_ 1)]]) in
utest liftLambdas liftSeq with expected using eqExpr in

let liftRecord = preprocess (urecord_ [("a", fapp), ("b", int_ 2)]) in
let expected = preprocess (bindall_ [
  fdef,
  urecord_ [
    ("a", app_ (var_ "f") (int_ 1)),
    ("b", int_ 2)]]) in
utest liftLambdas liftRecord with expected using eqExpr in

let liftRecordUpdate = preprocess (bindall_ [
  ulet_ "r" (urecord_ [("a", float_ 2.5), ("b", int_ 0)]),
  recordupdate_ (var_ "r") "b" fapp
  ]) in
let expected = preprocess (bindall_ [
  ulet_ "r" (urecord_ [("a", float_ 2.5), ("b", int_ 0)]),
  fdef,
  recordupdate_ (var_ "r") "b" (app_ (var_ "f") (int_ 1))]) in
utest liftLambdas liftRecordUpdate with expected using eqExpr in

let liftMatchTarget = preprocess (
  match_ fapp (pint_ 0)
    (int_ 1)
    (int_ 2)) in
let expected = preprocess (bindall_ [
  fdef,
  match_ (app_ (var_ "f") (int_ 1)) (pint_ 0)
    (int_ 1)
    (int_ 2)]) in
utest liftLambdas liftMatchTarget with expected using eqExpr in

let liftMatchThn = preprocess (
  match_ (int_ 3) (pint_ 3)
    fapp
    (int_ 0)) in
let expected = preprocess (bindall_ [
  fdef,
  match_ (int_ 3) (pint_ 3)
    (app_ (var_ "f") (int_ 1))
    (int_ 0)]) in
utest liftLambdas liftMatchThn with expected using eqExpr in

let liftMatchEls = preprocess (
  match_ (int_ 3) (pint_ 3)
    (int_ 0)
    fapp) in
let expected = preprocess (bindall_ [
  fdef,
  match_ (int_ 3) (pint_ 3)
    (int_ 0)
    (app_ (var_ "f") (int_ 1))]) in
utest liftLambdas liftMatchEls with expected using eqExpr in

let conAppLift = preprocess (bindall_ [
  type_ "Tree" [] (tyvariant_ []),
  condef_ "Leaf" (tyarrow_ tyint_ (tycon_ "Tree")),
  conapp_ "Leaf" fapp
]) in
let expected = preprocess (bindall_ [
  type_ "Tree" [] (tyvariant_ []),
  condef_ "Leaf" (tyarrow_ tyint_ (tycon_ "Tree")),
  fdef,
  conapp_ "Leaf" (app_ (var_ "f") (int_ 1))]) in

-- NOTE(larshum, 2022-09-15): Compare using eqString as equality of TmType has
-- not been implemented.
utest expr2str (liftLambdas conAppLift) with expr2str expected using eqString in

let anonymousFunctionLift = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (
    app_ (ulam_ "y" (addi_ (var_ "x") (var_ "y"))) (int_ 4))),
  app_ (var_ "f") (int_ 2)]) in
let expected = preprocess (bindall_ [
  ulet_ "t" (ulam_ "x" (ulam_ "y" (addi_ (var_ "x") (var_ "y")))),
  ulet_ "f" (ulam_ "x" (appf2_ (var_ "t") (var_ "x") (int_ 4))),
  app_ (var_ "f") (int_ 2)]) in
utest liftLambdas anonymousFunctionLift with expected using eqExpr in

let anonymousMapLift = preprocess (
  map_ (ulam_ "x" (addi_ (var_ "x") (int_ 1))) (seq_ [int_ 0, int_ 7])) in
let expected = preprocess (bindall_ [
  ulet_ "t" (ulam_ "x" (addi_ (var_ "x") (int_ 1))),
  map_ (var_ "t") (seq_ [int_ 0, int_ 7])]) in
utest liftLambdas anonymousMapLift with expected using eqExpr in

let recursiveSystem = preprocess (bindall_ [
  ulet_ "a" (int_ 1),
  ulet_ "b" (int_ 2),
  ulet_ "c" (int_ 5),
  ureclets_ [
    ("f", ulam_ "x" (addi_ (app_ (var_ "g") (var_ "x")) (var_ "a"))),
    ("g", ulam_ "y" (addi_ (app_ (var_ "h") (var_ "y")) (var_ "b"))),
    ("h", ulam_ "z" (addi_ (app_ (var_ "f") (var_ "z")) (var_ "c")))],
  unit_]) in
let expected = preprocess (bindall_ [
  ulet_ "a" (int_ 1),
  ulet_ "b" (int_ 2),
  ulet_ "c" (int_ 5),
  ureclets_ [
    ("f", ulams_ ["a", "b", "c", "x"] (
      addi_ (appSeq_ (var_ "g") [var_ "a", var_ "b", var_ "c", var_ "x"])
            (var_ "a"))),
    ("g", ulams_ ["a", "b", "c", "y"] (
      addi_ (appSeq_ (var_ "h") [var_ "a", var_ "b", var_ "c", var_ "y"])
            (var_ "b"))),
    ("h", ulams_ ["a", "b", "c", "z"] (
      addi_ (appSeq_ (var_ "f") [var_ "a", var_ "b", var_ "c", var_ "z"])
            (var_ "c")))],
  unit_]) in
utest liftLambdas recursiveSystem with expected using eqExpr in

let boundInMatchPat = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (
    match_ (var_ "x") (pvar_ "y")
      (bind_
        (ulet_ "g" (ulam_ "z" (addi_ (var_ "y") (var_ "z"))))
        (app_ (var_ "g") (var_ "x")))
      (int_ 0)))]) in
let expected = preprocess (bindall_ [
  ulet_ "g" (ulam_ "y" (ulam_ "z" (addi_ (var_ "y") (var_ "z")))),
  ulet_ "f" (ulam_ "x" (
    match_ (var_ "x") (pvar_ "y")
      (appf2_ (var_ "g") (var_ "y") (var_ "x"))
      (int_ 0)))]) in
utest liftLambdas boundInMatchPat with expected using eqExpr in

let nestedFreeVar = preprocess (bindall_ [
  ulet_ "f" (ulam_ "x" (bindall_ [
    ulet_ "g" (ulam_ "y" (bindall_ [
      ulet_ "h" (ulam_ "z" (addi_ (var_ "x") (var_ "z"))),
      app_ (var_ "h") (var_ "y")])),
    app_ (var_ "g") (var_ "x")]))]) in
let expected = preprocess (bindall_ [
  ulet_ "h" (ulam_ "x" (ulam_ "z" (addi_ (var_ "x") (var_ "z")))),
  ulet_ "g" (ulam_ "x" (ulam_ "y" (appf2_ (var_ "h") (var_ "x") (var_ "y")))),
  ulet_ "f" (ulam_ "x" (appf2_ (var_ "g") (var_ "x") (var_ "x")))]) in
utest liftLambdas nestedFreeVar with expected using eqExpr in

let letMultiParam = preprocess (bindall_ [
  ulet_ "a" (int_ 2),
  ulet_ "b" (int_ 6),
  ulet_ "f" (ulam_ "x" (
    addi_ (addi_ (var_ "a") (var_ "b")) (var_ "x"))),
  app_ (var_ "f") (int_ 7)]) in 
let expected = preprocess (bindall_ [
  ulet_ "a" (int_ 2),
  ulet_ "b" (int_ 6),
  ulet_ "f" (ulam_ "a" (ulam_ "b" (ulam_ "x" (
    addi_ (addi_ (var_ "a") (var_ "b")) (var_ "x"))))),
  appf3_ (var_ "f") (var_ "a") (var_ "b") (int_ 7)]) in
utest liftLambdas letMultiParam with expected using eqExpr in

let nestedMap = preprocess (bindall_ [
  ulet_ "s" (seq_ [seq_ [int_ 1, int_ 2, int_ 3]]),
  map_
    (ulam_ "s" (map_ (ulam_ "x" (addi_ (var_ "x") (int_ 1))) (var_ "s")))
    (var_ "s")]) in
let expected = preprocess (bindall_ [
  ulet_ "s" (seq_ [seq_ [int_ 1, int_ 2, int_ 3]]),
  ulet_ "t1" (ulam_ "x" (addi_ (var_ "x") (int_ 1))),
  ulet_ "t2" (ulam_ "s" (map_ (var_ "t1") (var_ "s"))),
  map_ (var_ "t2") (var_ "s")]) in
utest liftLambdas nestedMap with expected using eqExpr in

let nestedAnonymousLambdas = preprocess (bindall_ [
  ulet_ "s" (seq_ [int_ 1, int_ 2, int_ 3]),
  ulet_ "x" (int_ 0),
  map_
    (ulam_ "y" (bindall_ [
      ulet_ "s" (map_ (ulam_ "x" (addi_ (var_ "x") (var_ "y"))) (var_ "s")),
      ulet_ "s" (map_ (ulam_ "y" (addi_ (var_ "x") (var_ "y"))) (var_ "s")),
      ulet_ "s" (map_ (ulam_ "z" (addi_ (var_ "z") (int_ 1))) (var_ "s")),
      var_ "s"]))
    (var_ "s")]) in
let expected = preprocess (bindall_ [
  ulet_ "s" (seq_ [int_ 1, int_ 2, int_ 3]),
  ulet_ "x" (int_ 0),
  ulet_ "t1" (ulam_ "y" (ulam_ "x" (addi_ (var_ "x") (var_ "y")))),
  ulet_ "t2" (ulam_ "x" (ulam_ "y" (addi_ (var_ "x") (var_ "y")))),
  ulet_ "t3" (ulam_ "z" (addi_ (var_ "z") (int_ 1))),
  ulet_ "t4" (ulam_ "s" (ulam_ "x" (ulam_ "y" (bindall_ [
    ulet_ "s" (map_ (app_ (var_ "t1") (var_ "y")) (var_ "s")),
    ulet_ "s" (map_ (app_ (var_ "t2") (var_ "x")) (var_ "s")),
    ulet_ "s" (map_ (var_ "t3") (var_ "s")),
    var_ "s"])))),
  map_ (appf2_ (var_ "t4") (var_ "s") (var_ "x")) (var_ "s")]) in
utest liftLambdas nestedAnonymousLambdas with expected using eqExpr in

let nestedMultiArgLambda = preprocess (bindall_ [
  ulet_ "s" (seq_ [seq_ [int_ 1, int_ 2, int_ 3]]),
  map_
    (ulam_ "y"
      (foldl_ (ulam_ "acc" (ulam_ "e" (addi_ (var_ "acc") (var_ "e"))))
              (int_ 0) (var_ "y")))
    (var_ "s")]) in
let expected = preprocess (bindall_ [
  ulet_ "s" (seq_ [seq_ [int_ 1, int_ 2, int_ 3]]),
  ulet_ "t1" (ulam_ "acc" (ulam_ "e" (addi_ (var_ "acc") (var_ "e")))),
  ulet_ "t2" (ulam_ "y" (foldl_ (var_ "t1") (int_ 0) (var_ "y"))),
  map_ (var_ "t2") (var_ "s")]) in
utest liftLambdas nestedMultiArgLambda with expected using eqExpr in

let nestedReclets = preprocess (bindall_ [
  ulet_ "foo" (ulam_ "x" (ulam_ "y" (ulam_ "mylist" (
    if_ (eqi_ (var_ "x") (int_ 10))
        unit_
        (bindall_ [
          ureclet_ "inner_foo" (ulam_ "z" (
            if_ (eqi_ (var_ "y") (var_ "z"))
                (appf1_ (var_ "inner_foo") (addi_ (var_ "z") (int_ 1)))
                (bindall_ [
                  ureclet_ "deep_foo" (ulam_ "i" (bindall_ [
                    if_ (eqi_ (var_ "i") (var_ "z"))
                        (unit_)
                        (bindall_ [
                          ulet_ "" (get_ (var_ "mylist") (var_ "i")),
                          appf1_ (var_ "deep_foo")
                                 (addi_ (var_ "i")
                                        (int_ 1))
                        ])
                  ])),
                  appf1_ (var_ "deep_foo") (int_ 0)
                ])
          )),
          appf1_ (var_ "inner_foo") (int_ 10)
        ])
  )))),
  appf3_ (var_ "foo") (int_ 11) (int_ 12) (seq_ [int_ 1, int_ 2, int_ 3])
  ]) in
let expected = preprocess (bindall_ [
  ureclets_ [
    ("deep_foo", (ulam_ "mylist" (ulam_ "z" (ulam_ "i" (
      if_ (eqi_ (var_ "i") (var_ "z"))
          unit_
          (bindall_ [
            ulet_ "" (get_ (var_ "mylist") (var_ "i")),
            appf3_ (var_ "deep_foo")
                   (var_ "mylist")
                   (var_ "z")
                   (addi_ (var_ "i") (int_ 1))
          ])
    ))))),
    ("inner_foo", (ulam_ "y" (ulam_ "mylist" (ulam_ "z" (
              if_ (eqi_ (var_ "y") (var_ "z"))
                  (appf3_ (var_ "inner_foo")
                          (var_ "y")
                          (var_ "mylist")
                          (addi_ (var_ "z") (int_ 1)))
                  (appf3_ (var_ "deep_foo")
                           (var_ "mylist")
                           (var_ "z")
                           (int_ 0))
    )))))
  ],
  ulet_ "foo" (ulam_ "x" (ulam_ "y" (ulam_ "mylist" (
    if_ (eqi_ (var_ "x") (int_ 10))
        (unit_)
        (appf3_ (var_ "inner_foo") (var_ "y") (var_ "mylist") (int_ 10))
  )))),
  appf3_ (var_ "foo") (int_ 11) (int_ 12) (seq_ [int_ 1, int_ 2, int_ 3])
]) in
utest liftLambdas nestedReclets with expected using eqExpr in

let types = preprocess
  (ulet_ "f" (ulam_ "s"
    (bind_
      (ulet_ "g" (ulam_ "x" (snoc_ (var_ "s") (var_ "x"))))
      (foldl_ (uconst_ (CConcat ())) (seq_ []) (map_ (var_ "g") (var_ "s")))))) in
let expected = preprocess
  (bindall_ [
    ulet_ "g" (ulam_ "s" (ulam_ "x" (snoc_ (var_ "s") (var_ "x")))),
    ulet_ "f" (ulam_ "s"
      (foldl_ (uconst_ (CConcat ())) (seq_ []) (map_ (app_ (var_ "g") (var_ "s")) (var_ "s"))))]) in
utest liftLambdas types with expected using eqExpr in

-- NOTE(larshum, 2023-01-27): Tests that multiple functions capturing the same
-- free variable are given a distinct name for the captured variable.
let multiCaptures = preprocess (nulet_ f (nulam_ x (bindall_ [
  nulet_ g (nulam_ y (addi_ (nvar_ x) (nvar_ y))),
  nulet_ h (nulam_ z (addi_ (nvar_ x) (nvar_ z))),
  addi_ (app_ (nvar_ g) (int_ 2)) (app_ (nvar_ h) (int_ 4))
]))) in
let ast = liftLambdas multiCaptures in
let expected = preprocess (bindall_ [
  nulet_ h (nulam_ b (nulam_ z (addi_ (nvar_ b) (nvar_ z)))),
  nulet_ g (nulam_ a (nulam_ y (addi_ (nvar_ a) (nvar_ y)))),
  nulet_ f (nulam_ x (addi_ (appf2_ (nvar_ g) (nvar_ x) (int_ 2))
                            (appf2_ (nvar_ h) (nvar_ x) (int_ 4)))),
  unit_
]) in
utest ast with expected using eqExpr in

-- NOTE(larshum, 2023-01-27): Tests that multiple passes of the lambda lifting
-- behave in the expected way.
let nestedLetsInReclets = preprocess (nureclets_ [
  (f, nulam_ x (bindall_ [
    nulet_ g (nulam_ y (bindall_ [
      nulet_ h (nulam_ z (addi_ (nvar_ x) (nvar_ z))),
      nulam_ w (app_ (nvar_ h) (nvar_ w))
    ])),
    app_ (nvar_ g) (int_ 1)
  ]))
]) in
let ast = nameAnonymousFunctions nestedLetsInReclets in
let expected = preprocess (nureclets_ [
  (f, nulam_ x (bindall_ [
    nulet_ g (nulam_ y (bindall_ [
      nulet_ h (nulam_ z (addi_ (nvar_ x) (nvar_ z))),
      nulet_ a (nulam_ w (app_ (nvar_ h) (nvar_ w))),
      nvar_ a
    ])),
    app_ (nvar_ g) (int_ 1)
  ]))
]) in
utest ast with expected using eqExpr in
let state = solveSetEquationsDefault expected in
utest state.vars with setOfSeq nameCmp [x, y, z, w] using setEq in
utest state.sols with mapFromSeq nameCmp [
  (f, mapFromSeq nameCmp []),
  (g, mapFromSeq nameCmp [(x, tyint_)]),
  (h, mapFromSeq nameCmp [(x, tyint_)]),
  (a, mapFromSeq nameCmp [(x, tyint_)])
] using mapEq (mapEq eqType) in
match liftLambdasWithSolutions expected with (solutions, finalAst) in
let expected = preprocess (nureclets_ [
  (a, ulam_ "x1" (nulam_ w (appf2_ (nvar_ h) (var_ "x1") (nvar_ w)))),
  (h, ulam_ "x2" (nulam_ z (addi_ (var_ "x2") (nvar_ z)))),
  (g, ulam_ "x3" (nulam_ y (app_ (nvar_ a) (var_ "x3")))),
  (f, nulam_ x (appf2_ (nvar_ g) (nvar_ x) (int_ 1)))
]) in
utest finalAst with expected using eqExpr in

-- NOTE(larshum, 2023-01-27): Due to the renaming of captured variables, the
-- final solutions should be different from what we initially get from solving
-- the set equations.
utest state.sols with solutions using lam l. lam r. not (mapEq (mapEq eqType) l r) in

()
