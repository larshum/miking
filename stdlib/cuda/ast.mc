include "c/ast.mc"

lang CudaAst = CAst
    syn CExpr =
    | CudaEApp { fun: Name, args: [CExpr], blocks: CExpr, tpb: CExpr }
    
    syn CTop =
    | CudaFun { ret: CType, id: Name, params: [(CType,Name)], body: [CStmt], annot: [CudaAnnot] }

    syn CudaAnnot =
    | CudaExternC {}
    | CudaDevice  {}
    | CudaGlobal  {}
end
