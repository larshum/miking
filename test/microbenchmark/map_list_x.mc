
include "benchmarkcommon.mc"
include "string.mc"
include "common.mc"
include "list.mc"
include "map_n.ml"

mexpr

let mapf = lam n.
  listMap (addi 1) (listFromSeq (create n (lam i. i)))
in

-- iter (lam x. print (int2string x)) (mapf n);

bc_repeat (lam. mapf n)
