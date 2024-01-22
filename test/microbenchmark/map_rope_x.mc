
include "benchmarkcommon.mc"
include "string.mc"
include "common.mc"
include "rope.mc"
include "map_n.ml"

mexpr
use MExprRope in

let mapf = lam n.
  mapRope (addi 1) (createRope n (lam i. i))
in

-- iter (lam x. print (int2string x)) (mapf n);

bc_repeat (lam. mapf n)
