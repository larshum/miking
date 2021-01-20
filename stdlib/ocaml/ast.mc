include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"

lang OCamlRecord
  syn Expr =
  | OTmRecordDecl { ident: Name, fieldNames: [String], inexpr: Expr }

  sem smap_Expr_Expr (f : Expr -> a) =
  | OTmRecordDecl t -> OTmRecordDecl {t with inexpr = f t.inexpr}

  sem sfold_Expr_Expr (f : a -> b -> a) (acc : a) =
  | OTmRecordDecl t -> f acc t.inexpr
end

lang OCamlMatch
  syn Expr =
  | OTmMatch
    { target : Expr
    , arms : [(Pat, Expr)]
    }

  syn Pat =
end

lang OCamlTuple
  syn Expr =
  | OTmTuple { values : [Expr] }

  syn Pat =
  | OPTuple { pats : [Pat] }
end

lang OCamlData
  syn Expr =
  | OTmConApp { ident : Name, args : [Expr] }

  syn Pat =
  | OPCon { ident : Name, args : [Pat] }
end

lang OCamlAst = FunAst + LetAst + RecLetsAst + ArithIntAst + ShiftIntAst
                + ArithFloatAst + BoolAst + CmpIntAst + CmpFloatAst
                + CharAst + OCamlMatch + NamedPat + IntPat + CharPat
                + BoolPat + OCamlTuple + OCamlData
                + CharAst + FloatIntConversionAst
                + RecordAst + OCamlRecord
end

mexpr
()
