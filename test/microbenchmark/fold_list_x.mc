
include "benchmarkcommon.mc"
include "string.mc"
include "common.mc"
include "list.mc"
include "fold.ml"

mexpr

let foldf = lam n.
  listFoldl addi 0 (listFromSeq (create n (lam i. i)))
in

-- printLn (int2string (foldf n));

bc_repeat (lam. foldf n)
