
include "benchmarkcommon.mc"
include "rope.mc"

mexpr
use MExprRope in

recursive let sum = lam acc. lam s.
  if nullRope s then acc
  else
    let h = headRope s in
    let t = tailRope s in
    sum (addi acc h) t
in

let s = createRope 1000 (lam i. i) in
bc_repeat (lam. sum 0 s)
