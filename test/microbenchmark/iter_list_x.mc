
include "benchmarkcommon.mc"
include "list.mc"
include "iter.ml"

mexpr

let iterf = lam n.
  let li = listFromSeq (create n (lam i. i)) in
  listMap (lam. ()) li;
  ()
in

bc_repeat (lam. iterf n)
