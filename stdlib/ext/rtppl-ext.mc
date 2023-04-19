include "ext/file-ext.mc"

type Opaque
type TimeStampedValue = (Int, Opaque)

let toTimestampedValue : all a. (Int, a) -> (Int, Opaque) = lam tuple.
  match tuple with (ts, value) in
  (ts, unsafeCoerce value)

let timestampValue : all a. TimeStampedValue -> a = lam tsv.
  match tsv with (ts, value) in
  unsafeCoerce value

-- Functions for reading and writing time-stamped values (a tuple containing a
-- time-stamp and an opaque value), using a latest-value semantics.
external lvRead : Int -> TimeStampedValue
let lvRead = lam port. lvRead port
external lvWrite : Int -> TimeStampedValue -> ()
let lvWrite = lam port. lam tsv. lvWrite port tsv

-- NOTE(larshum, 2022-10-25): Special versions to handle floats in the same way
-- as in C (the others rely on OCaml marshalling which gives different results).
external lvReadFloat : Int -> TimeStampedValue
let lvReadFloat = lam port. lvReadFloat port
external lvWriteFloat : Int -> TimeStampedValue -> ()
let lvWriteFloat = lam port. lam tsv. lvWriteFloat port tsv

external readBinary : ReadChannel -> Opaque
let readBinary : all a. ReadChannel -> a =
  lam inChannel.
  unsafeCoerce (readBinary inChannel)

external writeBinary : WriteChannel -> Opaque -> ()
let writeBinary : all a. WriteChannel -> a -> () =
  lam outChannel. lam v.
  writeBinary outChannel (unsafeCoerce v)

type Signal = Int

-- Overrides the signal handler for a particular signal (for simplicity, the
-- signal is simply encoded as an integer).
external setSignalHandler : Signal -> (Signal -> ()) -> ()

-- External functions used for supporting the 'sdelay' keyword
type Timespec = (Int, Int)
external clockGetTime : () -> Timespec
external clockNanosleep : Timespec -> ()

-- Sets the priority of the process, returning the previous priority
external setMaxPriority : () -> Int
external setPriority : Int -> Int

-- Reads and writes to external pipes
external externalReadFloatPipe : String -> [(Timespec, Float)]
external externalWriteFloatPipe : String -> Float -> Timespec -> ()
external externalReadDistFloatRecordPipe : String -> Int -> [(Timespec, [(Float, Opaque)])]
external externalWriteDistFloatRecordPipe : String -> ([Opaque], [Float]) -> Timespec -> Int -> ()

mexpr

utest setSignalHandler 1 (lam. print "hello") with true using lam. lam. true in

()
