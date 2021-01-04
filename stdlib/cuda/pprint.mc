include "c/pprint.mc"
include "ast.mc"

lang CudaPrettyPrint = CudaAst + CPrettyPrint
    sem printCExpr (env: PprintEnv) =
    | CudaEApp { fun = fun, args = args, blocks = blocks, tpb = tpb } ->
      match pprintEnvGetStr env fun with (env,fun) then
        match mapAccumL printCExpr env args with (env,args) then
          match printCExpr env blocks with (env,blocksStr) then
            match printCExpr env tpb with (env, tpbStr) then
              (env, _par (join [fun, "<<<", blocksStr, ", ", tpbStr, ">>>(", (strJoin ", " args), ")"]))
            else never
          else never
        else never
      else never

    sem printCTop (indent: Int) (env: PprintEnv) =
    | CudaFun { ret = ret, id = id, params = params, body = body, annot = annot } ->
      let i = indent in
      let ii = pprintIncr indent in
      match pprintEnvGetStr env id with (env,id) then
        let f = lam env. lam t.
          match pprintEnvGetStr env t.1 with (env,t1) then
            printCDef env t.0 t1 (None ())
          else never
        in
        let annot = strJoin " " (map (printCudaAnnot env) annot) in
        match mapAccumL f env params with (env,params) then
          let params = join ["(", strJoin ", " params, ")"] in
          match printCType (join [id, params]) env ret with (env,ty) then
            match printCStmts ii env body with (env,body) then
              (env, join [annot, " ", ty, " {", pprintNewline ii, body, pprintNewline i, "}"])
            else never
          else never
        else never
      else never

    sem printCudaAnnot (env: PprintEnv) =
    | CudaExternC {} -> "extern \"C\""
    | CudaDevice  {} -> "__device__"
    | CudaGlobal  {} -> "__global__"
end
