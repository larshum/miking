include "array.mc"
include "list.mc"

lang MExprRopeRepr
  syn RopeRepr a =
  | Leaf (Array[a])
  | Slice {v : Array[a], ofs : Int, len : Int}
  | Concat {lhs : RopeRepr a, rhs : RopeRepr a, len : Int}

  sem _copyMutArray : all a. Array[a] -> Int -> Array[a] -> Int -> Int -> ()
  sem _copyMutArray src srcOfs dst dstOfs =
  | 0 -> ()
  | n ->
    setMutArray dst dstOfs (getMutArray src srcOfs);
    _copyMutArray src (addi srcOfs 1) dst (addi dstOfs 1) (subi n 1)

  sem lengthRopeRepr : all a. RopeRepr a -> Int
  sem lengthRopeRepr =
  | Leaf a -> lengthMutArray a
  | Slice {len = len} | Concat {len = len} -> len

  sem getRopeRepr : all a. Int -> RopeRepr a -> a
  sem getRopeRepr i =
  | Leaf a -> getMutArray a i
  | Slice {v = v, ofs = ofs} -> getMutArray v (addi ofs i)
  | Concat {lhs = lhs, rhs = rhs} ->
    let n = lengthRopeRepr lhs in
    if lti i n then getRopeRepr i lhs else getRopeRepr (subi i n) rhs

  sem collapseRopeRepr : all a. RopeRepr a -> Array[a]
  sem collapseRopeRepr =
  | Leaf a -> a
  | Slice {v = a, ofs = ofs, len = len} -> subMutArray a ofs len
  | s & (Concat {lhs = lhs, rhs = rhs, len = len}) ->
    let dst = createMutArray len (lam. getRopeRepr 0 s) in
    let st = listEmpty in
    let i = ref 0 in
    let st = listCons lhs (listCons rhs st) in
    recursive let rep = lam st.
      match st with Cons (s, st) then
        match s with Leaf a then
          let n = lengthMutArray a in
          _copyMutArray a 0 dst (deref i) n ;
          modref i (addi (deref i) n) ;
          rep st
        else match s with Slice {v = a, ofs = ofs, len = len} then
          _copyMutArray a ofs dst (deref i) len ;
          modref i (addi (deref i) len) ;
          rep st
        else match s with Concat {lhs = lhs, rhs = rhs} then
          rep (listCons lhs (listCons rhs st))
        else never
      else ()
    in
    rep st;
    dst

  sem setRopeRepr : all a. Int -> a -> RopeRepr a -> RopeRepr a
  sem setRopeRepr i v =
  | Leaf a ->
    let ap = createMutArray (lengthMutArray a) (lam i. getMutArray a i) in
    setMutArray ap i v;
    Leaf ap
  | Slice {v = a, ofs = ofs, len = len} ->
    let a = createMutArray len (lam i. getMutArray a (addi ofs i)) in
    setMutArray a i v;
    Leaf a
  | Concat t ->
    let n = lengthRopeRepr t.lhs in
    if lti i n then Concat {t with lhs = setRopeRepr i v t.lhs}
    else Concat {t with rhs = setRopeRepr (subi i n) v t.rhs}

  sem splitAtRopeRepr : all a. Int -> Int -> RopeRepr a -> (RopeRepr a, RopeRepr a)
  sem splitAtRopeRepr n i =
  | Leaf a ->
    (Slice {v = a, ofs = 0, len = i}, Slice {v = a, ofs = i, len = subi n i})
  | Slice {v = a, ofs = ofs, len = _} ->
    (Slice {v = a, ofs = ofs, len = i}, Slice {v = a, ofs = addi ofs i, len = subi n i})
  | rr & (Concat _) ->
    let a = collapseRopeRepr rr in
    (Slice {v = a, ofs = 0, len = i}, Slice {v = a, ofs = i, len = subi n i})

  sem subRopeRepr : all a. Int -> Int -> RopeRepr a -> RopeRepr a
  sem subRopeRepr ofs n =
  | Leaf a -> Slice {v = a, ofs = ofs, len = n}
  | Slice {v = v, ofs = o} -> Slice {v = v, ofs = addi o ofs, len = n}
  | rr & (Concat _) ->
    let a = collapseRopeRepr rr in
    Slice {v = a, ofs = ofs, len = n}

  sem iterRopeRepr : all a. (a -> ()) -> RopeRepr a -> ()
  sem iterRopeRepr f =
  | Leaf a -> iterMutArray f a
  | Slice t ->
    let n = addi t.ofs t.len in
    recursive let iterwork = lam i.
      if eqi i n then ()
      else f (getMutArray t.v i) ; iterwork (addi i 1)
    in iterwork t.ofs
  | Concat t -> iterRopeRepr f t.lhs ; iterRopeRepr f t.rhs

  sem iteriRopeRepr : all a. (Int -> a -> ()) -> RopeRepr a -> ()
  sem iteriRopeRepr f =
  | Leaf a -> iteriMutArray f a
  | Slice t ->
    let n = addi t.ofs t.len in
    recursive let iterwork = lam i.
      if eqi i n then ()
      else f i (getMutArray t.v i) ; iterwork (addi i 1)
    in iterwork t.ofs
  | Concat t -> iteriRopeRepr f t.lhs ; iteriRopeRepr f t.rhs
end

lang MExprRope = MExprRopeRepr
  type Rope a = Ref (RopeRepr a)

  sem _failCheck : all a. String -> a
  sem _failCheck =
  | msg -> error (concat "Rope error: " msg)

  sem createRope : all a. Int -> (Int -> a) -> Rope a
  sem createRope n =
  | f -> ref (Leaf (createMutArray n f))

  sem emptyRope : all a. () -> Rope a
  sem emptyRope =
  | _ -> ref (Leaf [| |])

  sem lengthRope : all a. Rope a -> Int
  sem lengthRope =
  | r -> lengthRopeRepr (deref r)

  sem collapseRope : all a. Rope a -> Rope a
  sem collapseRope =
  | r -> modref r (Leaf (collapseRopeRepr (deref r))); r

  sem getRope : all a. Rope a -> Int -> a
  sem getRope r =
  | i ->
    let n = lengthRope r in
    if or (lti i 0) (geqi i n) then _failCheck "Out of bounds access (get)"
    else getRopeRepr i (deref r)

  sem setRope : all a. Rope a -> Int -> a -> Rope a
  sem setRope r i =
  | v ->
    let n = lengthRope r in
    if or (lti i 0) (geqi i n) then _failCheck "Out of bounds access (set)"
    else ref (setRopeRepr i v (deref r))

  sem concatRope : all a. Rope a -> Rope a -> Rope a
  sem concatRope l =
  | r ->
    let ln = lengthRope l in
    let rn = lengthRope r in
    if eqi ln 0 then r
    else if eqi rn 0 then l
    else ref (Concat {lhs = deref l, rhs = deref r, len = addi ln rn})

  sem consRope : all a. a -> Rope a -> Rope a
  sem consRope v =
  | r -> concatRope (ref (Leaf [|v|])) r

  sem snocRope : all a. Rope a -> a -> Rope a
  sem snocRope r =
  | v -> concatRope r (ref (Leaf [|v|]))

  sem splitAtRope : all a. Rope a -> Int -> (Rope a, Rope a)
  sem splitAtRope r =
  | idx ->
    let n = lengthRope r in
    if lti idx 0 then _failCheck "Negative index not allowed (splitAt)"
    else if gti idx n then _failCheck "Index beyond end of rope (splitAt)"
    else if eqi idx 0 then (emptyRope (), r)
    else if eqi idx n then (r, emptyRope ())
    else
      match splitAtRopeRepr n idx (deref r) with (l, r) in
      (ref l, ref r)

  sem subRope : all a. Rope a -> Int -> Int -> Rope a
  sem subRope r ofs =
  | count ->
    let n = lengthRope r in
    if lti ofs 0 then _failCheck "Negative offset not allowed (sub)"
    else if lti count 0 then _failCheck "Negative length not allowed (sub)"
    else if gti n 0 then
      let count = if lti (subi n ofs) count then subi n ofs else count in
      ref (subRopeRepr ofs count (deref r))
    else emptyRope ()

  sem headRope : all a. Rope a -> a
  sem headRope =
  | r ->
    if eqi (lengthRope r) 0 then _failCheck "Head on empty rope"
    else getRope r 0

  sem tailRope : all a. Rope a -> Rope a
  sem tailRope =
  | r ->
    let n = lengthRope r in
    if eqi n 0 then _failCheck "Tail on empty rope"
    else subRope r 1 (subi n 1)

  sem nullRope : all a. Rope a -> Bool
  sem nullRope =
  | r -> eqi (lengthRope r) 0

  sem iterRope : all a. (a -> ()) -> Rope a -> ()
  sem iterRope f =
  | r -> iterRopeRepr f (deref r)

  sem iteriRope : all a. (Int -> a -> ()) -> Rope a -> ()
  sem iteriRope f =
  | r -> iteriRopeRepr f (deref r)

  sem mapRope : all a. all b. (a -> b) -> Rope a -> Rope b
  sem mapRope f =
  | r ->
    let a = collapseRopeRepr (deref r) in
    ref (Leaf (mapMutArray f a))

  sem mapiRope : all a. all b. (Int -> a -> b) -> Rope a -> Rope b
  sem mapiRope f =
  | r ->
    let a = collapseRopeRepr (deref r) in
    ref (Leaf (mapiMutArray f a))

  sem foldlRope : all a. all b. (a -> b -> a) -> a -> Rope b -> a
  sem foldlRope f acc =
  | r ->
    let a = collapseRopeRepr (deref r) in
    foldlMutArray f acc a

  sem foldrRope : all a. all b. (b -> a -> a) -> a -> Rope b -> a
  sem foldrRope f acc =
  | r ->
    let a = collapseRopeRepr (deref r) in
    foldrMutArray f acc a

  sem mapAccumLRope : all a. all b. all c. (a -> b -> (a, c)) -> a -> Rope b -> (a, Rope c)
  sem mapAccumLRope f acc =
  | r ->
    let a = collapseRopeRepr (deref r) in
    match mapAccumLMutArray f acc a with (acc, a) in
    (acc, ref (Leaf a))

  sem reverseRope : all a. Rope a -> Rope a
  sem reverseRope =
  | r ->
    let a = collapseRopeRepr (deref r) in
    let n = lengthMutArray a in
    ref (Leaf (createMutArray n (lam i. getMutArray a (subi (subi n i) 1))))

  sem equalRope : all a. (a -> a -> Bool) -> Rope a -> Rope a -> Bool
  sem equalRope eqf l =
  | r ->
    if eqi (lengthRope l) (lengthRope r) then
      match (collapseRopeRepr (deref l), collapseRopeRepr (deref r)) with (lhs, rhs) in
      eqMutArray eqf lhs rhs
    else false

  sem toSeqRope : all a. Rope a -> [a]
  sem toSeqRope =
  | r ->
    match deref (collapseRope r) with Leaf a then seqOfMutArray a
    else error "Invalid shape of collapsed rope"

  sem ofSeqRope : all a. [a] -> Rope a
  sem ofSeqRope =
  | s -> ref (Leaf (seqToMutArray s))

  sem toMutArrayRope : all a. Rope a -> Array[a]
  sem toMutArrayRope =
  | r ->
    match deref (collapseRope r) with Leaf a then a
    else error "Invalid shape of collapsed rope"

  sem ofMutArrayRope : all a. Array[a] -> Rope a
  sem ofMutArrayRope =
  | a -> ref (Leaf a)
end

mexpr

use MExprRope in

let eqRopeMutArray = lam eqf. lam r. lam a.
  eqMutArray eqf (toMutArrayRope r) a
in
recursive let strJoin = lam delim. lam strs.
  match strs with [x, y] then
    concat x (concat delim y)
  else match strs with [x] ++ rest then
    concat x (concat delim (strJoin delim rest))
  else ""
in
let join = lam strs. foldl concat [] strs in
let ppRopeMutArray = lam ppElem. lam l. lam r.
  join [
    "    LHS: [|", strJoin "," (map ppElem (seqOfMutArray (toMutArrayRope l))), "|]\n",
    "    RHS: [|", strJoin "," (map ppElem (seqOfMutArray r)), "|]"
  ]
in
let int2string = lam i. float2string (int2float i) in

let a = createRope 3 (lam i. i) in
let b = createRope 7 (lam i. subi 7 i) in
let c = createRope 2 (lam i. addi 3 i) in
utest a with [|0,1,2|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in
utest b with [|7,6,5,4,3,2,1|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in
utest concatRope a c with [|0,1,2,3,4|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in

utest getRope a 2 with 2 using eqi in
utest getRope (concatRope a c) 4 with 4 using eqi in
utest splitAtRope b 3 with ([|7,6,5|], [|4,3,2,1|])
using lam l. lam r. and (eqRopeMutArray eqi l.0 r.0) (eqRopeMutArray eqi l.1 r.1) in

utest lengthRope a with 3 in
utest lengthRope b with 7 in
utest lengthRope (concatRope a c) with 5 in

utest setRope a 2 4 with [|0,1,4|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in
utest a with [|0,1,2|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in

utest consRope 1 a with [|1,0,1,2|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in
utest consRope 'a' (emptyRope ()) with [|'a'|] using eqRopeMutArray eqc
else ppRopeMutArray (lam c. [c]) in
utest snocRope a 1 with [|0,1,2,1|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in

utest reverseRope a with [|2,1,0|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in
utest reverseRope (emptyRope ()) with [| |] using eqRopeMutArray eqi
else ppRopeMutArray int2string in

utest headRope a with 0 using eqi in
utest headRope b with 7 using eqi in
utest tailRope c with [|4|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in
utest tailRope b with [|6,5,4,3,2,1|] using eqRopeMutArray eqi
else ppRopeMutArray int2string in

()
