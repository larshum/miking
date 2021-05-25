include "mexpr/ast.mc"
include "mexpr/eq.mc"
include "mexpr/pprint.mc"
include "mexpr/type-annot.mc"

let isNonNegative = use MExprAst in
  lam t : Expr.
  match ty t with TyInt _ then
    match t with TmConst {val = CInt {val = n}} then
      geqi n 0
    else match t with TmApp {lhs = TmConst {val = CLength _}} then
      true
    else false
  else false

lang MExprRewrite = MExprAst + MExprEq
  sem rewriteTerm =
  -- if geqi a b then e1 else e2 -> if lti a b then e2 else e1
  | TmMatch ({target = TmApp {lhs = TmApp ({lhs = TmConst ({val = CGeqi ()} & op),
                                            rhs = arg1} & app),
                              rhs = arg2},
              pat = PatBool {val = true}} & t) ->
    let rewrittenTarget =
      TmApp {{app with lhs = TmApp {lhs = TmConst {op with val = CLti ()},
                                    rhs = arg1}}
                  with rhs = arg2}
    in
    rewriteTerm
      (TmMatch {{{t with target = rewriteTerm rewrittenTarget}
                    with thn = rewriteTerm t.els}
                    with els = rewriteTerm t.thn})
  -- lti n 1 -> eqi n 0, if n >= 0
  | TmApp ({lhs = TmApp {lhs = TmConst ({val = CLti _} & lhs), rhs = arg1},
                         rhs = TmConst ({val = CInt {val = 1}} & arg2)} & t) ->
    match t.ty with TyArrow {from = TyInt _, to = TyBool _} then
      if isNonNegative arg1 then
        TmApp {{t with lhs = TmApp {lhs = TmConst {lhs with val = CEqi ()},
                                    rhs = arg1}}
                  with rhs = TmConst {arg2 with val = CInt {val = 0}}}
      else TmApp t
    else TmApp t
  -- subsequence s n (length s) -> subsequence s n (subi (length s) n)
  | TmApp ({lhs = TmApp {lhs = TmApp {lhs = TmConst {val = CSubsequence _},
                                      rhs = arg1},
                         rhs = TmConst ({val = CInt {val = n}} & arg2)},
            rhs = TmApp ({lhs = TmConst {val = CLength _},
                          rhs = seqArg} & arg3)} & t) ->
    if and (eqExpr arg1 seqArg) (gti n 0) then
      TmApp {t with rhs = withType tyint_ (subi_ (TmApp arg3) (TmConst arg2))}
    else
      TmApp t
/-
  -- Transformation above if we had a specific node for subsequence constant
  | TmSubsequence ({s = s1, off = TmConst {val = CInt {val = n}},
                    len = TmLength {s = s2}} & t) ->
    if and (eqExpr s1 s2) (gti n 0) then
      TmSubsequence {t with len = subi_ t.len t.off}
    else
      TmSubsequence t
-/
  -- TODO: add rewrite rule into CMap constant (and to other ones as well)
  | t -> smap_Expr_Expr rewriteTerm t
end

lang TestLang = MExprRewrite + MExprTypeAnnot + MExprEq + MExprPrettyPrint

mexpr

use TestLang in

let t1 = typeAnnot (bindall_ [
  if_
    (geqi_ (length_ (seq_ [int_ 1, int_ 2])) (int_ 1))
    (int_ 1)
    (int_ 0)
]) in
let t2 = typeAnnot (bindall_ [
  if_
    (eqi_ (length_ (seq_ [int_ 1, int_ 2])) (int_ 0))
    (int_ 0)
    (int_ 1)
]) in
utest rewriteTerm t1 with t2 using eqExpr in

let s = nameSym "s" in
let t1 = typeAnnot (bindall_ [
  nulet_ s (seq_ [int_ 1, int_ 2, int_ 3]),
  subsequence_ (nvar_ s) (int_ 1) (length_ (nvar_ s))
]) in
let t2 = typeAnnot (bindall_ [
  nulet_ s (seq_ [int_ 1, int_ 2, int_ 3]),
  subsequence_ (nvar_ s) (int_ 1) (subi_ (length_ (nvar_ s)) (int_ 1))
]) in
printLn (expr2str t1); printLn (expr2str t2); printLn (expr2str (rewriteTerm t1));
utest rewriteTerm t1 with t2 using eqExpr in

()
