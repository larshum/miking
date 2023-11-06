open Ustring.Op

module type TENSOR = sig
  type 'a t

  val get_exn : 'a t -> int array -> 'a

  val set_exn : 'a t -> int array -> 'a -> unit

  val linear_get_exn : 'a t -> int -> 'a

  val linear_set_exn : 'a t -> int -> 'a -> unit

  val shape : 'a t -> int array

  val rank : 'a t -> int

  val size : 'a t -> int

  val reshape_exn : 'a t -> int array -> 'a t

  val slice_exn : 'a t -> int array -> 'a t

  val sub_exn : 'a t -> int -> int -> 'a t

  val copy : 'a t -> 'a t

  val transpose_exn : 'a t -> int -> int -> 'a t
end

module type GENERIC = sig
  include TENSOR

  val create : int array -> (int array -> 'a) -> 'a t
end

module type BARRAY = sig
  include TENSOR

  val uninit_int : int array -> int t

  val uninit_float : int array -> float t

  val create_int : int array -> (int array -> int) -> int t

  val create_float : int array -> (int array -> float) -> float t
end

module type UOP = sig
  type 'a t

  val iter_slice : (int -> 'a t -> unit) -> 'a t -> unit

  val to_data_array : 'a t -> 'a array

  val to_ustring : ('a -> ustring) -> 'a t -> ustring
end

module type BOP = sig
  type 'a t1

  type 'b t2

  val equal : ('a -> 'b -> bool) -> 'a t1 -> 'b t2 -> bool
end

let prod = Array.fold_left ( * ) 1

let cartesian_to_linear_idx shape idx =
  let rank = Array.length shape in
  let n = Array.length idx in
  let tmp_ofs = ref 0 in
  let tmp = ref 1 in
  for k = rank - 1 downto n do
    tmp := !tmp * shape.(k)
  done ;
  for k = n - 1 downto 0 do
    tmp_ofs := !tmp_ofs + (!tmp * idx.(k)) ;
    tmp := !tmp * shape.(k)
  done ;
  !tmp_ofs

let transpose create shape get_exn t dim0 dim1 =
  let shape' = shape t in
  let rank = Array.length shape' in
  if dim0 >= 0 && dim0 < rank && dim1 >= 0 && dim1 < rank then (
    let tmp = shape'.(dim0) in
    shape'.(dim0) <- shape'.(dim1) ;
    shape'.(dim1) <- tmp ;
    create shape' (fun idx ->
        let idx' = Array.copy idx in
        let tmp = idx'.(dim0) in
        idx'.(dim0) <- idx'.(dim1) ;
        idx'.(dim1) <- tmp ;
        get_exn t idx' ) )
  else raise (Invalid_argument "Tensor.transpose")

module Generic : GENERIC = struct
  type 'a t =
    {data: 'a array; shape: int array; rank: int; offset: int; size: int}

  let rank t = t.rank

  let shape t = Array.copy t.shape

  let size t = t.size

  let is_valid_index shape idx =
    let valid = ref true in
    Array.iteri (fun i n -> valid := !valid && n >= 0 && n < shape.(i)) idx ;
    !valid

  let get_exn t idx =
    if Array.length idx = rank t && is_valid_index t.shape idx then
      let linear_idx = cartesian_to_linear_idx t.shape idx + t.offset in
      t.data.(linear_idx)
    else raise (Invalid_argument "Tensor.Op_mseq_generic.get_exn")

  let set_exn t idx v =
    if is_valid_index t.shape idx then
      let linear_idx = cartesian_to_linear_idx t.shape idx + t.offset in
      t.data.(linear_idx) <- v
    else raise (Invalid_argument "Tensor.Op_mseq_generic.set_exn")

  let linear_get_exn t linear_idx =
    if linear_idx >= 0 && linear_idx < t.size then
      t.data.(linear_idx + t.offset)
    else raise (Invalid_argument "Tensor.Op_mseq_generic.linear_get_exn")

  let linear_set_exn t linear_idx v =
    if linear_idx >= 0 && linear_idx < t.size then
      t.data.(linear_idx + t.offset) <- v
    else raise (Invalid_argument "Tensor.Op_mseq_generic.linear_set_exn")

  let reshape_exn t shape =
    if t.size = prod shape then
      let rank = Array.length shape in
      {t with shape; rank}
    else raise (Invalid_argument "Tensor.Dense.reshape_exn")

  let slice_exn t slice =
    if Array.length slice = 0 then t
    else if is_valid_index t.shape slice then
      let n = Array.length slice in
      let offset = cartesian_to_linear_idx t.shape slice + t.offset in
      let rank = t.rank - n in
      let shape = if rank > 0 then Array.sub t.shape n rank else [||] in
      let size = prod shape in
      {t with offset; rank; shape; size}
    else raise (Invalid_argument "Tensor.Dense.slice_exn")

  let sub_exn t ofs len =
    if t.rank > 0 && ofs >= 0 && ofs + len <= t.shape.(0) then (
      let offset = cartesian_to_linear_idx t.shape [|ofs|] + t.offset in
      let shape = Array.copy t.shape in
      shape.(0) <- len ;
      {t with offset; size= prod shape; shape} )
    else raise (Invalid_argument "Tensor.Dense.sub_exn")

  (* Adoped from OCaml Bigarray implementation *)
  let rec loop t idx0 idx f dim shape =
    if dim = Array.length idx then
      if idx = idx0 then () else set_exn t idx (f idx)
    else
      for j = 0 to pred shape.(dim) do
        idx.(dim) <- j ;
        loop t idx0 idx f (succ dim) shape
      done

  let create shape f =
    let offset = 0 in
    let rank = Array.length shape in
    let size = prod shape in
    if size = 0 then
      let data = [||] in
      {data; rank; shape; offset; size}
    else
      let idx = Array.make rank 0 in
      let x0 = f idx in
      let data = Array.make size x0 in
      let t = {data; rank; shape; offset; size} in
      if rank = 0 then t
      else (
        loop t (Array.copy idx) idx f 0 shape ;
        t )

  let copy t =
    let data = Array.init t.size (fun i -> t.data.(i + t.offset)) in
    let shape = t.shape in
    let rank = t.rank in
    let offset = 0 in
    let size = t.size in
    {data; shape; rank; offset; size}

  let transpose_exn t dim0 dim1 = transpose create shape get_exn t dim0 dim1
end

module Barray : BARRAY = struct
  type opaque

  type elem_type = Int_type | Float_type

  type 'a t =
    { data: opaque
    ; shape: int array
    ; rank: int
    ; offset: int
    ; size: int
    ; element_type: elem_type }

  external ext_tensor_init : int -> int -> opaque = "tensor_init"

  external ext_tensor_get : opaque -> int -> elem_type -> 'a = "tensor_get"

  external ext_tensor_set : opaque -> int -> 'a -> elem_type -> unit
    = "tensor_set"

  external ext_tensor_copy : opaque -> int -> opaque = "tensor_copy"

  let is_valid_index shape idx =
    let valid = ref true in
    Array.iteri (fun i n -> valid := !valid && n >= 0 && n < shape.(i)) idx ;
    !valid

  let get_exn t idx =
    if Array.length idx = t.rank && is_valid_index t.shape idx then
      let linear_idx = cartesian_to_linear_idx t.shape idx + t.offset in
      ext_tensor_get t.data linear_idx t.element_type
    else raise (Invalid_argument "Tensor.Barray.get_exn")

  let set_exn t idx v =
    if Array.length idx = t.rank && is_valid_index t.shape idx then
      let linear_idx = cartesian_to_linear_idx t.shape idx + t.offset in
      ext_tensor_set t.data linear_idx v t.element_type
    else raise (Invalid_argument "Tensor.Barray.set_exn")

  let linear_get_exn t lidx =
    if lidx >= 0 && lidx < t.size then
      ext_tensor_get t.data lidx t.element_type
    else raise (Invalid_argument "Tensor.Barray.linear_get_exn")

  let linear_set_exn t lidx v =
    if lidx >= 0 && lidx < t.size then
      ext_tensor_set t.data lidx v t.element_type
    else raise (Invalid_argument "Tensor.Barray.linear_set_exn")

  let rank t = t.rank

  let shape t = t.shape

  let size t = t.size

  let copy src = {src with data= ext_tensor_copy src.data src.size}

  let reshape_exn t shape =
    if t.size = prod shape then
      let rank = Array.length shape in
      {t with shape; rank}
    else raise (Invalid_argument "Tensor.Barray.reshape_exn")

  let slice_exn t slice =
    if Array.length slice = 0 then t
    else if is_valid_index t.shape slice then
      let n = Array.length slice in
      let offset = cartesian_to_linear_idx t.shape slice + t.offset in
      let rank = t.rank - n in
      let shape = if rank > 0 then Array.sub t.shape n rank else [||] in
      let size = prod shape in
      {t with offset; rank; shape; size}
    else raise (Invalid_argument "Tensor.Barray.slice_exn")

  let sub_exn t ofs len =
    if t.rank > 0 && ofs >= 0 && ofs + len <= t.shape.(0) then (
      let offset = cartesian_to_linear_idx t.shape [|ofs|] + t.offset in
      let shape = Array.copy t.shape in
      shape.(0) <- len ;
      {t with offset; size= prod shape; shape} )
    else raise (Invalid_argument "Tensor.Barray.sub_exn")

  let uninit_int shape =
    let sz = prod shape in
    { data= ext_tensor_init sz 0
    ; shape
    ; rank= Array.length shape
    ; offset= 0
    ; size= sz
    ; element_type= Int_type }

  let uninit_float shape =
    let sz = prod shape in
    { data= ext_tensor_init sz 1
    ; shape
    ; rank= Array.length shape
    ; offset= 0
    ; size= sz
    ; element_type= Float_type }

  let fill t f =
    if t.rank = 0 then (
      ext_tensor_set t.data 0 (f [||]) t.element_type ;
      t )
    else
      let cidx = Array.make t.rank 0 in
      let rec increment cidx dim =
        cidx.(dim) <- cidx.(dim) + 1 ;
        if cidx.(dim) = t.shape.(dim) then (
          cidx.(dim) <- 0 ;
          increment cidx (dim - 1) )
      in
      cidx.(t.rank - 1) <- -1 ;
      for i = 0 to t.size - 1 do
        increment cidx (t.rank - 1) ;
        ext_tensor_set t.data i (f cidx) t.element_type
      done ;
      t

  let create_int shape f = fill (uninit_int shape) f

  let create_float shape f = fill (uninit_float shape) f

  let create shape f = function
    | Int_type ->
        create_int shape f
    | Float_type ->
        create_float shape f

  let transpose_exn t dim0 dim1 =
    let shape = Array.copy t.shape in
    if dim0 >= 0 && dim0 < t.rank && dim1 >= 0 && dim1 < t.rank then (
      let tmp = shape.(dim0) in
      shape.(dim0) <- shape.(dim1) ;
      shape.(dim1) <- tmp ;
      let f idx =
        let idx' = Array.copy idx in
        let tmp = idx'.(dim0) in
        idx'.(dim0) <- idx'.(dim1) ;
        idx'.(dim1) <- tmp ;
        get_exn t idx'
      in
      create shape f t.element_type )
    else raise (Invalid_argument "Tensor.Barray.transpose")
end

module Uop (T : TENSOR) : UOP with type 'a t = 'a T.t = struct
  type 'a t = 'a T.t

  let iter_slice f t =
    if T.rank t = 0 then f 0 t
    else
      let n = (T.shape t).(0) in
      for i = 0 to n - 1 do
        f i (T.slice_exn t [|i|])
      done

  let to_data_array t =
    let n = T.size t in
    let t' = T.reshape_exn t [|n|] in
    Array.init n (fun i -> T.get_exn t' [|i|])

  let to_ustring el2str =
    let rec recur indent t =
      let rank = T.rank t in
      if rank = 0 then el2str (T.get_exn t [||])
      else if rank = 1 then (
        let elems = ref (us "") in
        let n = (T.shape t).(0) in
        for i = 0 to n - 1 do
          let e = if i < n - 1 then us ", " else us "" in
          elems := !elems ^. recur (us "") (T.slice_exn t [|i|]) ^. e
        done ;
        us "[" ^. !elems ^. us "]" )
      else
        let n = (T.shape t).(0) in
        let newindent = indent ^. us "\t" in
        let elems = ref (us "") in
        for i = 0 to n - 1 do
          let e = if i < n - 1 then us ",\n" ^. newindent else us "" in
          elems := !elems ^. recur newindent (T.slice_exn t [|i|]) ^. e
        done ;
        us "[\n" ^. newindent ^. !elems ^. us "\n" ^. indent ^. us "]"
    in
    recur (us "")
end

module Bop (T1 : TENSOR) (T2 : TENSOR) :
  BOP with type 'a t1 = 'a T1.t and type 'b t2 = 'b T2.t = struct
  type 'a t1 = 'a T1.t

  type 'b t2 = 'b T2.t

  let equal eq t1 t2 =
    if T1.shape t1 = T2.shape t2 then (
      let n = T1.size t1 in
      let v1 = T1.reshape_exn t1 [|n|] in
      let v2 = T2.reshape_exn t2 [|n|] in
      let tmp = ref true in
      let i = ref 0 in
      while !tmp && !i < n do
        tmp := eq (T1.get_exn v1 [|!i|]) (T2.get_exn v2 [|!i|]) ;
        incr i
      done ;
      !tmp )
    else false
end

module Uop_generic = Uop (Generic)
module Uop_barray = Uop (Barray)
module Bop_generic_generic = Bop (Generic) (Generic)
module Bop_barray_barray = Bop (Barray) (Barray)
module Bop_generic_barray = Bop (Generic) (Barray)
module Bop_barray_generic = Bop (Barray) (Generic)
