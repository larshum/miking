open Ustring

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

module Generic : GENERIC

module Barray : BARRAY

module Uop (T : TENSOR) : UOP with type 'a t = 'a T.t

module Bop (T1 : TENSOR) (T2 : TENSOR) :
  BOP with type 'a t1 = 'a T1.t and type 'b t2 = 'b T2.t

module Uop_generic : UOP with type 'a t = 'a Generic.t

module Uop_barray : UOP with type 'a t = 'a Barray.t

module Bop_generic_generic :
  BOP with type 'a t1 = 'a Generic.t and type 'b t2 = 'b Generic.t

module Bop_barray_barray :
  BOP with type 'a t1 = 'a Barray.t and type 'b t2 = 'b Barray.t

module Bop_generic_barray :
  BOP with type 'a t1 = 'a Generic.t and type 'b t2 = 'b Barray.t

module Bop_barray_generic :
  BOP with type 'a t1 = 'a Barray.t and type 'b t2 = 'b Generic.t
