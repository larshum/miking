include "map.mc"
include "ocaml/ast.mc"

let tyts_ = tytuple_ [tyint_, tyunknown_]
let impl = lam arg : {expr : String, ty : Type }.
  [ { expr = arg.expr, ty = arg.ty, libraries = ["rtppl-support"], cLibraries = ["rt"] } ]

let timespec = otytuple_ [tyint_, tyint_]

let rtpplExtMap =
  use OCamlTypeAst in
  mapFromSeq cmpString [
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
    ( "externalSeqToOcamlArray"
    , impl { expr = "Rtppl.seq_to_ocaml_array"
           , ty = tyarrow_ (tyseq_ tyunknown_) (otyarray_ tyunknown_) } ),
    ( "externalOcamlArrayToSeq"
    , impl { expr = "Rtppl.ocaml_array_to_seq"
           , ty = tyarrow_ (otyarray_ tyunknown_) (tyseq_ tyunknown_) } ),
    ( "externalReadFloatPipe"
    , impl { expr = "Rtppl.read_float_named_pipe"
           , ty = tyarrow_ otystring_ (otyarray_ (otytuple_ [timespec, tyfloat_])) } ),
    ( "externalWriteFloatPipe"
    , impl { expr = "Rtppl.write_float_named_pipe"
           , ty = tyarrows_ [otystring_, tyfloat_, timespec, otyunit_] } ),
    ( "externalReadDistFloatRecordPipe"
    , impl { expr = "Rtppl.read_dist_float_record_named_pipe"
           , ty = tyarrows_ [otystring_, tyint_,
               otyarray_ (otytuple_ [timespec, otyarray_ (otytuple_ [tyfloat_, tyunknown_])])] } ),
    ( "externalWriteDistFloatRecordPipe"
    , impl { expr = "Rtppl.write_dist_float_record_named_pipe"
           , ty = tyarrows_ [otystring_,
               otytuple_ [otyarray_ tyunknown_, otyarray_ tyfloat_], timespec, tyint_, otyunit_] } )
  ]
