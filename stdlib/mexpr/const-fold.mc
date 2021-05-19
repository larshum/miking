-- Performs constant folding and constant propagation of MExpr terms.

include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"
include "mexpr/eq.mc"
include "mexpr/eval.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-annot.mc"
include "map.mc"
include "name.mc"

lang ConstantFold = MExprEval
  sem isConstantTerm (env : Map Name Expr) =

  sem foldTerm (env : Map Name Expr) =

  sem evalIfConstant (env : Map Name Expr) =
  | t ->
    if isConstantTerm env t then
      withType (ty t) (eval {env = env} t)
    else smap_Expr_Expr (foldTerm env) t
end

lang VarConstantFold = ConstantFold + VarAst + ConstAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmVar t -> mapMem t.ident env

  sem foldTerm (env : Map Name Expr) =
  | TmVar t ->
    match mapLookup t.ident env with Some expr then
      expr
    else TmVar t
end

lang AppConstantFold = ConstantFold + AppAst + ConstAst + FunTypeAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmApp t -> and (isConstantTerm env t.lhs) (isConstantTerm env t.rhs)

  sem foldTerm (env : Map Name Expr) =
  | (TmApp {ty = ty}) & t ->
    match ty with TyArrow _ then
      smap_Expr_Expr (foldTerm env) t
    else
      evalIfConstant env (smap_Expr_Expr (foldTerm env) t)
end

lang LamConstantFold = LamAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmLam _ -> false

  sem foldTerm (env : Map Name Expr) =
  | (TmLam _) & t -> smap_Expr_Expr (foldTerm env) t
end

lang LetConstantFold = ConstantFold + LetAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmLet t -> and (isConstantTerm env t.body) (isConstantTerm env t.inexpr)

  sem foldTerm (env : Map Name Expr) =
  | TmLet t ->
    if isConstantTerm env (TmLet t) then
      withType t.ty (eval {env = env} (TmLet t))
    else
      let body = foldTerm env t.body in
      if isConstantTerm env body then
        foldTerm (mapInsert t.ident body env) t.inexpr
      else
        TmLet {{t with body = body}
                  with inexpr = foldTerm env t.inexpr}
end

lang RecLetsConstantFold = RecLetsAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmRecLets _ -> false

  sem foldTerm (env : Map Name Expr) =
  | (TmRecLets _) & t -> smap_Expr_Expr (foldTerm env) t
end

lang ConstConstantFold =
  IntAst + ArithIntAst + ShiftIntAst + FloatAst + ArithFloatAst +
  FloatIntConversionAst + BoolAst + CmpIntAst + CmpFloatAst + CharAst +
  CmpCharAst + IntCharConversionAst + FloatStringConversionAst + SymbAst +
  CmpSymbAst + SeqOpAst + FileOpAst + IOAst + RandomNumberGeneratorAst +
  SysAst + TimeAst + RefOpAst + MapAst + TensorOpAst + BootParserAst

  sem isConstant =
  | CInt _ -> true
  | CAddi _ -> true
  | CSubi _ -> true
  | CMuli _ -> true
  | CDivi _ -> true
  | CNegi _ -> true
  | CModi _ -> true
  | CSlli _ -> true
  | CSrli _ -> true
  | CSrai _ -> true
  | CFloat _ -> true
  -- NOTE(larshum, 2021-05-18): cannot fold floating-point arithmetic
  -- operations as the precision may differ between processing units.
  | CAddf _ -> false
  | CSubf _ -> false
  | CMulf _ -> false
  | CDivf _ -> false
  | CNegf _ -> false
  | CFloorfi _ -> true
  | CCeilfi _ -> true
  | CRoundfi _ -> true
  | CInt2float _ -> true
  | CBool _ -> true
  | CEqi _ -> true
  | CNeqi _ -> true
  | CLti _ -> true
  | CGti _ -> true
  | CLeqi _ -> true
  | CGeqi _ -> true
  | CEqf _ -> true
  | CLtf _ -> true
  | CLeqf _ -> true
  | CGtf _ -> true
  | CGeqf _ -> true
  | CNeqf _ -> true
  | CChar _ -> true
  | CEqc _ -> true
  | CInt2Char _ -> true
  | CChar2Int _ -> true
  | CString2float _ -> true
  | CFloat2string _ -> true
  | CSymb _ -> false
  | CGensym _ -> false
  | CSym2hash _ -> false
  | CEqsym _ -> false
  | CSet _ -> true
  | CGet _ -> true
  | CCons _ -> true
  | CSnoc _ -> true
  | CConcat _ -> true
  | CLength _ -> true
  | CReverse _ -> true
  | CCreate _ -> true
  | CSplitAt _ -> true
  | CSubsequence _ -> true
  | CFileRead _ -> false
  | CFileWrite _ -> false
  | CFileExists _ -> false
  | CFileDelete _ -> false
  | CPrint _ -> false
  | CDPrint _ -> false
  | CReadLine _ -> false
  | CReadBytesAsString _ -> false
  | CRandIntU _ -> false
  | CRandSetSeed _ -> false
  | CExit _ -> false
  | CError _ -> false
  | CArgv _ -> false
  | CCommand _ -> false
  | CWallTimeMs _ -> false
  | CSleepMs _ -> false
  | CRef _ -> false
  | CModRef _ -> false
  | CDeRef _ -> false
  | CMapEmpty _ -> false
  | CMapInsert _ -> false
  | CMapRemove _ -> false
  | CMapFindWithExn _ -> false
  | CMapFindOrElse _ -> false
  | CMapFindApplyOrElse _ -> false
  | CMapBindings _ -> false
  | CMapSize _ -> false
  | CMapMem _ -> false
  | CMapAny _ -> false
  | CMapMap _ -> false
  | CMapMapWithKey _ -> false
  | CMapFoldWithKey _ -> false
  | CMapEq _ -> false
  | CMapCmp _ -> false
  | CMapGetCmpFun _ -> false
  | CTensorCreateInt _ -> true
  | CTensorCreateFloat _ -> true
  | CTensorCreate _ -> true
  | CTensorGetExn _ -> true
  | CTensorSetExn _ -> false
  | CTensorRank _ -> true
  | CTensorShape _ -> true
  | CTensorReshapeExn _ -> false
  | CTensorCopyExn _ -> false
  | CTensorSliceExn _ -> false
  | CTensorSubExn _ -> false
  | CTensorIteri _ -> false
  | CBootParserParseMExprString _ -> false
  | CBootParserParseMCoreFile _ -> false
  | CBootParserGetId _ -> false
  | CBootParserGetTerm _ -> false
  | CBootParserGetType _ -> false
  | CBootParserGetString _ -> false
  | CBootParserGetInt _ -> false
  | CBootParserGetFloat _ -> false
  | CBootParserGetListLength _ -> false
  | CBootParserGetConst _ -> false
  | CBootParserGetPat _ -> false
  | CBootParserGetInfo _ -> false

  sem isConstantTerm (env : Map Name Expr) =
  | TmConst t -> isConstant t.val

  sem foldTerm (env : Map Name Expr) =
  | TmConst t -> TmConst t
end

lang SeqConstantFold = ConstantFold + SeqAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmSeq t -> all (isConstantTerm env) t.tms

  sem foldTerm (env : Map Name Expr) =
  | (TmSeq _) & t -> smap_Expr_Expr (foldTerm env) t
end

lang RecordConstantFold = ConstantFold + RecordAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmRecord t -> mapAll (isConstantTerm env) t.bindings
  | TmRecordUpdate t -> and (isConstantTerm t.rec) (isConstantTerm t.value)

  sem foldTerm (env : Map Name Expr) =
  | (TmRecord _) & t -> smap_Expr_Expr (foldTerm env) t
  | (TmRecordUpdate _) & t -> smap_Expr_Expr (foldTerm env) t
end

lang TypeConstantFold = TypeAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmType _ -> false

  sem foldTerm (env : Map Name Expr) =
  | (TmType _) & t -> smap_Expr_Expr (foldTerm env) t
end

lang DataConstantFold = ConstantFold + DataAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmConDef _ -> false
  | TmConApp _ -> false

  sem foldTerm (env : Map Name Expr) =
  | (TmConDef _) & t -> smap_Expr_Expr (foldTerm env) t
  | (TmConApp _) & t -> smap_Expr_Expr (foldTerm env) t
end

lang MatchConstantFold = ConstantFold + MatchAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmMatch t -> and (isConstantTerm env t.target)
                     (and (isConstantTerm env t.thn) (isConstantTerm env t.els))

  sem foldTerm (env : Map Name Expr) =
  | TmMatch t ->
    if isConstantTerm env (TmMatch t) then
      withType t.ty (eval {env = env} (TmMatch t))
    else if isConstantTerm env t.target then
      let target = withType (ty t.target) (eval {env = env} t.target) in
      match tryMatch {env = env} target t.pat with Some _ then
        evalIfConstant env t.thn
      else
        evalIfConstant env t.els
    else smap_Expr_Expr (foldTerm env) (TmMatch t)
end

lang UtestConstantFold = UtestAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmUtest _ -> false

  sem foldTerm (env : Map Name Expr) =
  | TmUtest t -> foldTerm env t.next
end

lang NeverConstantFold = NeverAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmNever _ -> true

  sem foldTerm (env : Map Name Expr) =
  | TmNever t -> TmNever t
end

lang ExtConstantFold = ExtAst
  sem isConstantTerm (env : Map Name Expr) =
  | TmExt _ -> false

  sem foldTerm (env : Map Name Expr) =
  | (TmExt _) & t -> smap_Expr_Expr (foldTerm env) t
end

lang MExprConstantFold =
  VarConstantFold + AppConstantFold + LamConstantFold + LetConstantFold +
  RecLetsConstantFold + ConstConstantFold + SeqConstantFold +
  RecordConstantFold + TypeConstantFold + DataConstantFold +
  MatchConstantFold + UtestConstantFold + NeverConstantFold + ExtConstantFold

  sem foldConstants =
  | t -> foldTerm (mapEmpty nameCmp) t
end

lang TestLang = MExprConstantFold + MExprEq + MExprSym + MExprTypeAnnot

mexpr

use TestLang in

let foldConstants = lam t : Expr.
  foldConstants (typeAnnot (symbolize t))
in

utest foldConstants (addi_ (int_ 1) (int_ 2)) with int_ 3 using eqExpr in
utest foldConstants (slli_ (int_ 3) (int_ 1)) with int_ 6 using eqExpr in
utest foldConstants (eqi_ (int_ 2) (int_ 3)) with false_ using eqExpr in
utest foldConstants (addf_ (int2float_ (int_ 1)) (float_ 2.3))
with addf_ (float_ 1.0) (float_ 2.3) using eqExpr in

let nestedIntegerOperations =
  divi_ (modi_ (addi_ (int_ 7) (char2int_ (char_ '0'))) (int_ 10)) (int_ 2)
in
utest foldConstants nestedIntegerOperations with int_ 2 using eqExpr in

let nextChar_ = lam c.
  int2char_ (addi_ (char2int_ c) (int_ 1))
in
utest foldConstants (eqc_ (nextChar_ (char_ 'a')) (char_ 'b')) with true_ using eqExpr in
utest foldConstants (eqf_ (float_ 2.718) (float_ 2.718)) with true_ using eqExpr in
utest foldConstants (string2float_ (str_ "2.0")) with float_ 2.0 using eqExpr in

utest foldConstants (get_ (seq_ [int_ 1, int_ 2]) (int_ 0)) with int_ 1 using eqExpr in
utest foldConstants (set_ (seq_ [int_ 1, int_ 2, int_ 3]) (int_ 2) (int_ 7))
with seq_ [int_ 1, int_ 2, int_ 7] using eqExpr in
utest foldConstants (cons_ (char_ 'x') (str_ "abc")) with str_ "xabc" using eqExpr in
utest foldConstants (length_ (concat_ (str_ "abc") (str_ "xyz")))
with int_ 6 using eqExpr in

let t = symbolize (bindall_ [
  ulet_ "x" (muli_ (int_ 4) (int_ 7)),
  ulet_ "y" (int_ 5),
  divi_ (var_ "x") (var_ "y")
]) in
utest foldConstants t with int_ 5 using eqExpr in

let x = nameSym "x" in
let y = nameSym "y" in
let t = bindall_ [
  nulet_ x (float_ 2.718),
  nulet_ y (addf_ (float_ 1.718) (int2float_ (int_ 1))),
  eqf_ (nvar_ x) (nvar_ y)
] in
utest foldConstants t
with bind_ (nulet_ y (addf_ (float_ 1.718) (float_ 1.0)))
           (eqf_ (float_ 2.718) (nvar_ y)) using eqExpr in

let t = bindall_ [
  ulet_ "seq" (str_ "ok"),
  if_ (eqi_ (length_ (var_ "seq")) (int_ 0))
    (str_ "empty")
    (reverse_ (var_ "seq"))
] in
utest foldConstants t with str_ "ko" using eqExpr in

let t = bindall_ [
  ulet_ "seq" (str_ "ok"),
  if_ (eqi_ (length_ (var_ "seq")) (int_ 0))
    (print_ (str_ "empty"))
    (print_ (str_ "not empty"))
] in
utest foldConstants t with print_ (str_ "not empty") using eqExpr in

let t = bindall_ [
  ulet_ "seq" (str_ "ok"),
  match_ (var_ "seq")
    (pseqtot_ [pchar_ 'o', pchar_ 'k'])
    (print_ (str_ "ok"))
    (print_ (str_ "not ok"))
] in
utest foldConstants t with print_ (str_ "ok") using eqExpr in

let a = nameSym "a" in
let b = nameSym "b" in
let c = nameSym "c" in
let t = bindall_ [
  nulet_ a (int_ 30),
  nulet_ b (subi_ (int_ 9) (divi_ (nvar_ a) (int_ 5))),
  nulet_ c (muli_ (nvar_ b) (int_ 4)),
  nulet_ c (if_ (gti_ (nvar_ c) (int_ 10)) (subi_ (nvar_ c) (int_ 10)) (nvar_ c)),
  muli_ (nvar_ c) (divi_ (int_ 60) (nvar_ a))
] in
utest foldConstants t with int_ 4 using eqExpr in

()
