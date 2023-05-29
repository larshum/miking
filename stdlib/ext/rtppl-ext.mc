type Signal = Int

-- Overrides the signal handler for the SIGINT signal.
external setSigintHandler : (Signal -> ()) -> ()

type Timespec = (Int, Int)
external getMonotonicTime : () -> Timespec
external getWallClockTime : () -> Timespec
external getProcessCpuTime : () -> Timespec
external clockNanosleep : Timespec -> ()

-- Sets the priority of the process, returning the previous priority
external setMaxPriority : () -> Int
external setPriority : Int -> Int

-- Opens and closes file descriptors to pipes
external externalOpenFileNonblocking : String -> Int
external externalCloseFileDescriptor : Int -> ()

type Opaque

-- Reads and writes to external pipes
external externalReadFloatPipe : Int -> [(Timespec, Float)]
external externalWriteFloatPipe : Int -> (Timespec, Float) -> ()
external externalReadDistFloatPipe : Int -> [(Timespec, [(Float, Float)])]
external externalWriteDistFloatPipe : Int -> (Timespec, ([Float], [Float])) -> ()
external externalReadDistFloatRecordPipe : Int -> Int -> [(Timespec, [(Float, Opaque)])]
external externalWriteDistFloatRecordPipe : Int -> Int -> (Timespec, ([Opaque], [Float])) -> ()

external externalBatchedInference : (() -> Opaque) -> Timespec -> [Opaque]

mexpr

utest setSigintHandler (lam. print "hello") with true using lam. lam. true in

()
