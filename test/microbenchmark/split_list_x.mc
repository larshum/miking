
include "benchmarkcommon.mc"
include "list.mc"

mexpr

recursive let sum = lam acc. lam s.
  if listNil s then acc
  else
    match s with Cons (h, t) in
    sum (addi acc h) t
in

let s = listFromSeq (create 1000 (lam i. i)) in
bc_repeat (lam. sum 0 s)
