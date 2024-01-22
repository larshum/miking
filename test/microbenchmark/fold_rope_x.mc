
include "benchmarkcommon.mc"
include "string.mc"
include "common.mc"
include "rope.mc"
include "fold.ml"

mexpr

use MExprRope in

let foldf = lam n.
  foldlRope addi 0 (createRope n (lam i. i))
in

-- printLn (int2string (foldf n));

bc_repeat (lam. foldf n)
