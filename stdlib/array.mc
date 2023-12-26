include "option.mc"

let seqOfMutArray : all a. Array[a] -> [a] = lam a.
  create (lengthMutArray a) (lam i. getMutArray a i)

let seqToMutArray : all a. [a] -> Array[a] = lam s.
  createMutArray (length s) (lam i. get s i)

let copyMutArray : all a. Array[a] -> Array[a] = lam a.
  let n = lengthMutArray a in
  createMutArray n (lam i. getMutArray a i)

let subMutArray : all a. Array[a] -> Int -> Int -> Array[a] =
  lam a. lam ofs. lam len.
  createMutArray len (lam i. getMutArray a (addi i ofs))

let mapMutArray : all a. all b. (a -> b) -> Array[a] -> Array[b] = lam f. lam a.
  let n = lengthMutArray a in
  createMutArray n (lam i. f (getMutArray a i))

let mapiMutArray : all a. all b. (Int -> a -> b) -> Array[a] -> Array[b] = lam f. lam a.
  let n = lengthMutArray a in
  createMutArray n (lam i. f i (getMutArray a i))

let foldlMutArray : all a. all b. (a -> b -> a) -> a -> Array[b] -> a =
  lam f. lam acc. lam a.
  let n = lengthMutArray a in
  recursive let work = lam acc. lam i.
    if eqi i n then acc
    else work (f acc (getMutArray a i)) (addi i 1)
  in
  work acc 0

let foldrMutArray : all a. all b. (b -> a -> a) -> a -> Array[b] -> a =
  lam f. lam acc. lam a.
  let n = lengthMutArray a in
  recursive let work = lam acc. lam i.
    if geqi i 0 then work (f (getMutArray a i) acc) (subi i 1)
    else acc
  in work acc (subi n 1)

let mapAccumLMutArray : all a. all b. all c. (a -> b -> (a, c)) -> a -> Array[b] -> (a, Array[c]) =
  lam f. lam acc. lam a.
  let n = lengthMutArray a in
  if gti n 0 then
    match f acc (getMutArray a 0) with (acc, y) in
    let dst = createMutArray n (lam. y) in
    recursive let work = lam acc. lam i.
      if eqi i n then acc
      else
        match f acc (getMutArray a i) with (acc, y) in
        setMutArray dst i y ;
        work acc (addi i 1)
    in
    (work acc 1, dst)
  else
    (acc, [| |])

let iterMutArray : all a. (a -> ()) -> Array[a] -> () = lam f. lam a.
  let n = lengthMutArray a in
  recursive let work = lam i.
    if eqi i n then ()
    else f (getMutArray a i) ; work (addi i 1)
  in work 0

let iteriMutArray : all a. (Int -> a -> ()) -> Array[a] -> () = lam f. lam a.
  let n = lengthMutArray a in
  recursive let work = lam i.
    if eqi i n then ()
    else f i (getMutArray a i) ; work (addi i 1)
  in work 0

let optionFoldlMMutArray : all a. all b. (a -> b -> Option a) -> a -> Array[b] -> Option a =
  lam f. lam acc. lam a.
  let n = lengthMutArray a in
  recursive let work = lam acc. lam i.
    if eqi i n then Some acc
    else
      let res = f acc (getMutArray a i) in
      match res with Some acc then work acc (addi i 1)
      else None ()
  in work acc 0

let cmpMutArray : all a. (a -> a -> Int) -> Array[a] -> Array[a] -> Int =
  lam cmpfn. lam l. lam r.
  recursive let work = lam i. lam n.
    if eqi i n then 0
    else
      let c = cmpfn (getMutArray l i) (getMutArray r i) in
      if eqi c 0 then work (addi i 1) n
      else c
  in
  let n1 = lengthMutArray l in
  let n2 = lengthMutArray r in
  let ndiff = subi n1 n2 in
  if eqi ndiff 0 then work 0 n1
  else ndiff

let eqMutArray : all a. (a -> a -> Bool) -> Array[a] -> Array[a] -> Bool =
  lam eqfn. lam l. lam r.
  recursive let work = lam i. lam n.
    if eqi i n then true
    else if eqfn (getMutArray l i) (getMutArray r i) then work (addi i 1) n
    else false
  in
  let n1 = lengthMutArray l in
  let n2 = lengthMutArray r in
  if eqi n1 n2 then work 0 n1 else false

mexpr

let a = createMutArray 3 (lam i. i) in
let f = lam acc. lam x. (addi acc x, addi x 1) in
match mapAccumLMutArray f 0 a with (acc, b) in
utest acc with 3 in
utest b with [|1,2,3|] using eqMutArray eqi in

()
