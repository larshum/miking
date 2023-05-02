include "map.mc"
include "ocaml/ast.mc"

let tyts_ = tytuple_ [tyint_, tyunknown_]
let impl = lam arg : {expr : String, ty : Type }.
  [ { expr = arg.expr, ty = arg.ty, libraries = ["rtppl-support"], cLibraries = [] } ]

let timespec = otytuple_ [tyint_, tyint_]
let readDistTy = lam ty. otyarray_ (otytuple_ [tyfloat_, ty])
let writeDistTy = lam ty. otytuple_ [otyarray_ ty, otyarray_ tyfloat_]

let rtpplExtMap =
  use OCamlTypeAst in
  mapFromSeq cmpString [
    ( "setSignalHandler"
    , impl { expr = "Rtppl.set_signal_handler"
           , ty = tyarrows_ [tyint_, tyarrow_ tyint_ otyunit_, otyunit_] } ),
    ( "getMonotonicTime"
    , impl { expr = "Rtppl.get_monotonic_time"
           , ty = tyarrow_ otyunit_ timespec} ),
    ( "getWallClockTime"
    , impl { expr = "Rtppl.get_wall_clock_time"
           , ty = tyarrow_ otyunit_ timespec} ),
    ( "clockNanosleep"
    , impl { expr = "Rtppl.clock_nanosleep"
           , ty = tyarrow_ timespec otyunit_ } ),
    ( "setMaxPriority"
    , impl { expr = "Rtppl.set_max_priority"
           , ty = tyarrow_ otyunit_ tyint_ } ),
    ( "setPriority"
    , impl { expr = "Rtppl.set_priority"
           , ty = tyarrow_ tyint_ tyint_ } ),
    ( "externalOpenFileNonblocking"
    , impl { expr = "Rtppl.open_file_nonblocking"
           , ty = tyarrow_ otystring_ tyint_ } ),
    ( "externalCloseFileDescriptor"
    , impl { expr = "Rtppl.close_file_descriptor"
           , ty = tyarrow_ tyint_ otyunit_ } ),
    ( "externalReadFloatPipe"
    , impl { expr = "Rtppl.read_float_named_pipe"
           , ty = tyarrow_ tyint_ (otyarray_ (otytuple_ [timespec, tyfloat_])) } ),
    ( "externalWriteFloatPipe"
    , impl { expr = "Rtppl.write_float_named_pipe"
           , ty = tyarrows_ [tyint_, otytuple_ [timespec, tyfloat_], otyunit_] } ),
    ( "externalReadDistFloatPipe"
    , impl { expr = "Rtppl.read_dist_float_named_pipe"
           , ty = tyarrow_ tyint_ (otyarray_ (otytuple_ [timespec, readDistTy tyfloat_])) } ),
    ( "externalWriteDistFloatPipe"
    , impl { expr = "Rtppl.write_dist_float_named_pipe"
           , ty = tyarrows_ [tyint_, otytuple_ [timespec, writeDistTy tyfloat_], otyunit_] } ),
    ( "externalReadDistFloatRecordPipe"
    , impl { expr = "Rtppl.read_dist_float_record_named_pipe"
           , ty = tyarrows_ [tyint_, tyint_, otyarray_ (otytuple_ [timespec, readDistTy tyunknown_])] } ),
    ( "externalWriteDistFloatRecordPipe"
    , impl { expr = "Rtppl.write_dist_float_record_named_pipe"
           , ty = tyarrows_ [tyint_, tyint_, otytuple_ [timespec, writeDistTy tyunknown_], otyunit_] } ),
    ( "externalBatchedInference"
    , impl { expr = "Rtppl.rtppl_batched_inference"
           , ty = tyarrows_ [tyarrow_ otyunit_ tyunknown_, timespec, tyint_, otylist_ tyunknown_] } )
  ]
