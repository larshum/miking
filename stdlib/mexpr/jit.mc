include "mexpr/ast.mc"
include "ocaml/mcore.mc"

type Dyn

type LibCompileResult = {
  cleanup : () -> (),
  libPath : String
}

let compileOcamlLibrary : String -> [String] -> [String] -> String -> LibCompileResult =
  lam id. lam libs. lam clibs. lam ocamlProg.
  let distinctStr = distinct eqString in
  let libStr = strJoin " " (distinctStr (cons "boot" libs)) in
  let flagStr =
    let base = ":standard -w -a " in
    let clibIncludes = map (concat "-cclib -l") (distinctStr clibs) in
    concat base (strJoin " " clibIncludes)
  in
  let dunefile = strJoin "\n" [
    join ["(env (dev (flags (", flagStr, ")) (ocamlc_flags (-without-runtime))))"],
    join ["(library (name ", id, ") (libraries ", libStr, "))"]
  ] in
  let td = sysTempDirMake () in
  let dir = sysTempDirName td in
  let tempfile = lam f. sysJoinPath dir f in

  writeFile (tempfile (concat id ".ml")) ocamlProg;
  writeFile (tempfile "dune-project") "(lang dune 2.0)";
  writeFile (tempfile "dune") dunefile;

  let command = ["dune", "build"] in
  let r = sysRunCommand command "" dir in
  (if neqi r.returncode 0 then
    sysTempDirDelete td;
    exit 1
  else ());

  { cleanup = lam. sysTempDirDelete td (); ()
  , libPath = tempfile (join ["_build/default/", id, ".cmxs"]) }

-- Performs an OCaml compilation which returns the path to the '.cmxs' file,
-- such that we can dynamically link the compiled code.
let jitCompile : all a. String -> Expr -> a = lam extId. lam e.
  let p =
    use MCoreCompileLang in
    compileMCore e (mkEmptyHooks (compileOcamlLibrary extId))
  in
  loadLibraries [] p.libPath;
  p.cleanup ();
  unsafeCoerce (getExternal extId)

let _jitCompiled = ref (mapEmpty nameCmp)

let _compileExpr : all a. Name -> Expr -> a = lam id. lam e.
  match mapLookup id (deref _jitCompiled) with Some f then f
  else
    let nameToStr = lam id.
      let s = nameGetStr id in
      match nameGetSym id with Some sym then
        join [s, "_", int2string (sym2hash sym)]
      else s
    in
    let extId = concat "mexpr_jit_" (nameToStr id) in
    let e = bind_ (ulet_ "f" e) (addExternal_ (str_ extId) (var_ "f")) in
    let f = jitCompile extId e in
    modref _jitCompiled (mapInsert id f (deref _jitCompiled));
    f

lang MExprJIT = MExprAst
  syn Const =
  | CJitExt (Int, Type, Dyn)

  sem isJitSupportedType : Type -> Bool
  sem isJitSupportedType =
  | TyBool _ -> true
  | TyInt _ -> true
  | TyFloat _ -> true
  | TyChar _ -> true
  | TyArrow {from = from, to = to} ->
    and (isJitSupportedType from) (isJitSupportedType to)
  | _ -> false

  sem boundInPat : Set Name -> Pat -> Set Name
  sem boundInPat bound =
  | PatNamed {ident = PName id} -> setInsert id bound
  | PatNamed _ -> bound
  | p -> sfold_Pat_Pat boundInPat bound p

  sem isEligibleForJit : Set Name -> Expr -> Bool
  sem isEligibleForJit bound =
  | TmVar t ->
    if setMem t.ident bound then true
    else mapMem t.ident (deref _jitCompiled)
  | TmApp {lhs = lhs, rhs = rhs} ->
    and (isEligibleForJit bound lhs) (isEligibleForJit bound rhs)
  | TmLam {ident = ident, body = body} ->
    let bound = setInsert ident bound in
    isEligibleForJit bound body
  | TmLet {ident = ident, body = body, inexpr = inexpr} ->
    if isEligibleForJit bound body then
      let bound = setInsert ident bound in
      isEligibleForJit bound inexpr
    else false
  | TmRecLets t ->
    if
      foldl
        (lam acc. lam bind.
          if acc then isEligibleForJit bound bind.body else false)
        true t.bindings
    then
      let bound =
        foldl
          (lam bound. lam bind. setInsert bind.ident bound)
          bound t.bindings in
      isEligibleForJit bound t.inexpr
    else false
  | TmConst _ -> true
  | TmSeq {tms = tms} ->
    foldl
      (lam acc. lam e.
        if acc then isEligibleForJit bound e else false)
      true tms
  | TmRecord {bindings = bindings} ->
    mapFoldWithKey
      (lam acc. lam. lam e.
        if acc then isEligibleForJit bound e else false)
      true bindings
  | TmRecordUpdate t ->
    and (isEligibleForJit bound t.rec) (isEligibleForJit bound t.value)
  | TmType t -> isEligibleForJit bound t.inexpr
  | TmConDef t -> isEligibleForJit bound t.inexpr
  | TmConApp t -> isEligibleForJit bound t.body
  | TmMatch t ->
    if isEligibleForJit bound t.target then
      let thnBound = boundInPat bound t.pat in
      and (isEligibleForJit thnBound t.thn) (isEligibleForJit bound t.els)
    else false
  | TmUtest t -> false
  | TmNever _ -> true
  | TmExt t -> false
  | _ -> false

  sem replaceCompiledReferences : Expr -> Expr
  sem replaceCompiledReferences =
  | TmVar t ->
    match mapLookup t.ident (deref _jitCompiled) with Some f then
      unsafeCoerce f
    else TmVar t
  | t -> smap_Expr_Expr replaceCompiledReferences t

  sem typeArity : Type -> Int
  sem typeArity =
  | TyAll {ty = ty} -> typeArity ty
  | TyArrow {to = to} -> addi (typeArity to) 1
  | _ -> 0

  sem jitCompile : Name -> Expr -> Option Expr
  sem jitCompile id =
  | e ->
    let info = infoTm e in
    let ty = tyTm e in
    if and (isJitSupportedType ty) (isEligibleForJit (mapEmpty nameCmp) e) then
      let e = replaceCompiledReferences e in
      let f = _compileExpr id e in
      Some (TmConst {val = CJitExt (typeArity ty, ty, f), info = info,
                     ty = TyUnknown {info = info}})
    else None ()

  sem unwrapConst =
  | TmConst (t & {val = CJitExt (0, TyBool _, e)}) ->
    TmConst {t with val = CBool {val = unsafeCoerce e}}
  | TmConst (t & {val = CJitExt (0, TyInt _, e)}) ->
    TmConst {t with val = CInt {val = unsafeCoerce e}}
  | TmConst (t & {val = CJitExt (0, TyChar _, e)}) ->
    TmConst {t with val = CChar {val = unsafeCoerce e}}
  | TmConst (t & {val = CJitExt (0, TyFloat _, e)}) ->
    TmConst {t with val = CFloat {val = unsafeCoerce e}}
  | e -> e

  sem delta info arg =
  | CJitExt (n & !0, TyArrow {from = TyBool _, to = to}, f) ->
    match arg with TmConst (t & {val = CBool {val = b}}) then
      unwrapConst (TmConst {t with val = CJitExt (subi n 1, to, unsafeCoerce f b)})
    else errorSingle [info] ""
  | CJitExt (n & !0, TyArrow {from = TyInt _, to = to}, f) ->
    match arg with TmConst (t & {val = CInt {val = i}}) then
      unwrapConst (TmConst {t with val = CJitExt (subi n 1, to, unsafeCoerce f i)})
    else errorSingle [info] ""
  | CJitExt (n & !0, TyArrow {from = TyChar _, to = to}, f) ->
    match arg with TmConst (t & {val = CChar {val = c}}) then
      unwrapConst (TmConst {t with val = CJitExt (subi n 1, to, unsafeCoerce f c)})
    else errorSingle [info] ""
  | CJitExt (n & !0, TyArrow {from = TyFloat _, to = to}, f) ->
    match arg with TmConst (t & {val = CFloat {val = fl}}) then
      unwrapConst (TmConst {t with val = CJitExt (subi n 1, to, unsafeCoerce f fl)})
    else errorSingle [info] ""
  | CJitExt _ -> errorSingle [info] ""
end
