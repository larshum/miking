-- Transforms all uses of the 'inAccelerate' keyword to boolean values, and
-- then simplifies any branches where these are used such that only either case
-- is available. The keyword is equal to 'true' in the accelerated AST and
-- 'false' in the sequential AST.

include "ast.mc"
include "mexpr/eq.mc"

lang PMExprEliminateInAccelerate = PMExprAst
  sem replaceInAccelerate : Bool -> Expr -> Expr
  sem replaceInAccelerate val =
  | TmInAccelerate t -> TmConst {val = CBool {val = val}, ty = t.ty, info = t.info}
  | e -> smap_Expr_Expr (replaceInAccelerate val) e

  sem simplifyBranches : Expr -> Expr
  sem simplifyBranches =
  | TmMatch {
      target = TmConst {val = CBool {val = b1}},
      pat = PatBool {val = b2}, thn = thn, els = els
    } ->
    if eqBool b1 b2 then thn
    else els
  | e -> smap_Expr_Expr simplifyBranches e

  sem eliminateInAccelerate : Expr -> Expr -> (Expr, Expr)
  sem eliminateInAccelerate seqAst =
  | accAst ->
    let seqAst = replaceInAccelerate false seqAst in
    let accAst = replaceInAccelerate true accAst in
    (simplifyBranches seqAst, simplifyBranches accAst)
end

lang TestLang = PMExprEliminateInAccelerate + MExprEq
end

mexpr

use TestLang in

let e = bindall_ [
  if_ (inAccelerate_ ())
    (int_ 1)
    (int_ 2)
] in
utest simplifyBranches (replaceInAccelerate true e) with int_ 1 using eqExpr in
utest simplifyBranches (replaceInAccelerate false e) with int_ 2 using eqExpr in

()
