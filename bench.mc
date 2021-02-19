include "mexpr/ast-builder.mc"
include "mexpr/pprint.mc"
include "seq.mc"

mexpr

use MExprPrettyPrint in

let reps = 10 in

let bench = lam n.
  let p = create n (lam _. ulet_ "x" (int_ 1)) in
  let p = bindall_ (snoc p (int_ 0)) in
  recursive let helper = lam acc. lam i.
    if eqi i reps then divf (foldl addf 0. acc) (int2float reps)
    else
      let t1 = wallTimeMs () in
      let s = expr2str p in
      let t2 = wallTimeMs () in
      let t = divf (subf t2 t1) 1000. in
      helper (cons t acc) (addi i 1)
  in
  helper [] 0
in

let resetJoinTimer = lam _.
  modref _joinTimer 0.0
in

let printJoinTimer = lam _.
  let _ = print "join: " in
  let _ = dprint (divf (deref _joinTimer) (int2float reps)) in
  print "\n"
in

let sizes = [25, 50, 100, 200] in
let _ = iter (lam sz.
  let _ = resetJoinTimer () in
  let t = bench sz in
  let _ = print "time (sz=" in
  let _ = dprint sz in
  let _ = print "): " in
  let _ = dprint t in
  let _ = print "\n" in
  let _ = printJoinTimer () in
  let _ = print "\n" in
  ()
) sizes in
()
