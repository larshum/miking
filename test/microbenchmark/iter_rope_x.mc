
include "benchmarkcommon.mc"
include "rope.mc"
include "iter.ml"

mexpr

use MExprRope in

let iterf = lam n.
  iterRope (lam. ()) (createRope n (lam i. i))
in

bc_repeat (lam. iterf n)
