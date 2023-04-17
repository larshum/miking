include "map.mc"
include "ocaml/ast.mc"

let tyts_ = tytuple_ [tyint_, tyunknown_]
let impl = lam arg : {expr : String, ty : Type }.
  [ { expr = arg.expr, ty = arg.ty, libraries = ["rtppl-support"], cLibraries = ["rt"] } ]

let timespec = otytuple_ [tyint_, tyint_]

let rtpplExtMap =
  use OCamlTypeAst in
  mapFromSeq cmpString [
    ("lvRead", impl { expr = "Rtppl.lv_read", ty = tyarrow_ tyint_ tyts_ }),
    ("lvWrite", impl { expr = "Rtppl.lv_write", ty = tyarrows_ [tyint_, tyts_, otyunit_] }),
    ("lvReadFloat", impl { expr = "Rtppl.lv_read_float", ty = tyarrow_ tyint_ tyts_ }),
    ("lvWriteFloat", impl { expr = "Rtppl.lv_write_float", ty = tyarrows_ [tyint_, tyts_, otyunit_] }),
    ( "readBinary"
    , impl { expr = "Rtppl.read_binary"
           , ty = tyarrows_ [otyvarext_ "in_channel" [], tyunknown_] }),
    ("writeBinary"
    , impl { expr = "Rtppl.write_binary"
           , ty = tyarrows_ [otyvarext_ "out_channel" [], tyunknown_, otyunit_] }),
    ( "setSignalHandler"
    , impl { expr = "Rtppl.set_signal_handler"
           , ty = tyarrows_ [tyint_, tyarrow_ tyint_ otyunit_, otyunit_] } ),
    ( "clockGetTime"
    , impl { expr = "Rtppl.clock_get_time"
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
    ( "externalReadFloatPipe"
    , impl { expr = "Rtppl.read_float_named_pipe"
           , ty = tyarrow_ otystring_ (otyarray_ (otytuple_ [timespec, tyfloat_])) } ),
    ( "externalReadDistFloatRecordPipe"
    , impl { expr = "Rtppl.read_dist_float_record_named_pipe"
           , ty = tyarrow_ otystring_ (otyarray_ (otytuple_ [timespec, tyunknown_])) } ),
    ( "externalWriteFloatPipe"
    , impl { expr = "Rtppl.write_float_named_pipe"
           , ty = tyarrows_ [otystring_, tyfloat_, timespec, otyunit_] } ),
    ( "externalWriteDistFloatRecordPipe"
    , impl { expr = "Rtppl.write_dist_float_record_named_pipe"
           , ty = tyarrows_ [otystring_, tyunknown_, timespec, tyint_, otyunit_] } )
  ]
