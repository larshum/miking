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

type Opaque

-- Reads and writes to external pipes
external externalReadFloatPipe : String -> [(Timespec, Float)]
external externalWriteFloatPipe : String -> Float -> Timespec -> ()
external externalReadDistFloatRecordPipe : String -> Int -> [(Timespec, [(Float, Opaque)])]
external externalWriteDistFloatRecordPipe : String -> ([Opaque], [Float]) -> Timespec -> Int -> ()

mexpr

utest setSignalHandler 1 (lam. print "hello") with true using lam. lam. true in

()
