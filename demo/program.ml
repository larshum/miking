let _ =
  let libs = Boot.Intrinsics.Mseq.Helpers.of_list
    [Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "owl")] in
  let cmxs = Boot.Intrinsics.Mseq.Helpers.of_ustring
    (Boot.Ustring.from_utf8 "./_build/default/demo/extdyn.cmxs") in
  Boot.Intrinsics.Ext.load_libraries libs cmxs;;

type v_record'1653 =
  | CRec'1652 of {l0: Obj.t;l1: Obj.t};;
let v_numFailed =
  Obj.magic
    ref
    0;;
let v_join =
  fun v_seqs'1457 ->
    Obj.magic
      Boot.Intrinsics.Mseq.Helpers.fold_left
      Boot.Intrinsics.Mseq.concat
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [|  |])
      v_seqs'1457;;
let v_printLn =
  fun v_s'1459 ->
    Obj.magic
      Boot.Intrinsics.IO.print
      (Obj.magic
         Boot.Intrinsics.Mseq.concat
         v_s'1459
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (10) |]));;
let v_int2string =
  fun v_n'1461 ->
    let rec v_int2string_rechelper'1462 =
        fun v_n'1463 ->
          if
            Obj.magic
              (Obj.magic
                 ((<) : int -> int -> bool)
                 v_n'1463
                 10)
          then
            Obj.magic
              Boot.Intrinsics.Mseq.Helpers.of_array
              [| (Obj.magic
                  (Obj.magic
                     Fun.id
                     (Obj.magic
                        Int.add
                        v_n'1463
                        (Obj.magic
                           Fun.id
                           48)))) |]
          else
            Obj.magic
              (let v_d'1464 =
                 Obj.magic
                   Boot.Intrinsics.Mseq.Helpers.of_array
                   [| (Obj.magic
                       (Obj.magic
                          Fun.id
                          (Obj.magic
                             Int.add
                             (Obj.magic
                                Int.rem
                                v_n'1463
                                10)
                             (Obj.magic
                                Fun.id
                                48)))) |]
               in
               Obj.magic
                 Boot.Intrinsics.Mseq.concat
                 (Obj.magic
                    v_int2string_rechelper'1462
                    (Obj.magic
                       Int.div
                       v_n'1463
                       10))
                 v_d'1464)
    in if
      Obj.magic
        (Obj.magic
           ((<) : int -> int -> bool)
           v_n'1461
           0)
    then
      Obj.magic
        Boot.Intrinsics.Mseq.cons
        45
        (Obj.magic
           v_int2string_rechelper'1462
           (Obj.magic
              Int.neg
              v_n'1461))
    else
      Obj.magic
        (Obj.magic
           v_int2string_rechelper'1462
           v_n'1461);;
let rec v_strJoin =
    fun v_delim'1467 ->
      fun v_strs'1468 ->
        if
          Obj.magic
            (Obj.magic
               Int.equal
               (Obj.magic
                  Boot.Intrinsics.Mseq.length
                  v_strs'1468)
               0)
        then
          Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [|  |]
        else
          Obj.magic
            (if
               Obj.magic
                 (Obj.magic
                    Int.equal
                    (Obj.magic
                       Boot.Intrinsics.Mseq.length
                       v_strs'1468)
                    1)
             then
               Obj.magic
                 Boot.Intrinsics.Mseq.head
                 v_strs'1468
             else
               Obj.magic
                 (Obj.magic
                    Boot.Intrinsics.Mseq.concat
                    (Obj.magic
                       Boot.Intrinsics.Mseq.concat
                       (Obj.magic
                          Boot.Intrinsics.Mseq.head
                          v_strs'1468)
                       v_delim'1467)
                    (Obj.magic
                       v_strJoin
                       v_delim'1467
                       (Obj.magic
                          Boot.Intrinsics.Mseq.tail
                          v_strs'1468))));;
let v_eqSeq =
  fun v_eq'1469 ->
    let rec v_work'1470 =
        fun v_as'1471 ->
          fun v_bs'1472 ->
            let v_pair'1473 =
              CRec'1652 { l0 =
                  (Obj.repr
                    v_as'1471);
                l1 =
                  (Obj.repr
                    v_bs'1472) }
            in
            let v_matchBody'1637 =
              fun nv_ ->
                true
            in
            let v_matchBody'1638 =
              fun v_a'1474 ->
                fun v_as'1475 ->
                  fun v_b'1476 ->
                    fun v_bs'1477 ->
                      fun nv_ ->
                        if
                          Obj.magic
                            (Obj.magic
                               v_eq'1469
                               v_a'1474
                               v_b'1476)
                        then
                          Obj.magic
                            v_work'1470
                            v_as'1475
                            v_bs'1477
                        else
                          Obj.magic
                            false
            in
            match
              Obj.magic
                (let v__target'1654 =
                   v_pair'1473
                 in
                 let
                   CRec'1652 ({l0 = v_0'1655;l1 = v_1'1656})
                 =
                   Obj.magic
                     Obj.magic
                     v__target'1654
                 in
                 Option.Some (v_0'1655, v_1'1656))
            with
            | Option.Some (v_field'1639, v_field'1640) ->
                (match
                   Obj.magic
                     (let v__target'1657 =
                        v_field'1639
                      in
                      if
                        Obj.magic
                          Boot.Intrinsics.Mseq.null
                          v__target'1657
                      then
                        Option.None
                      else
                        Obj.magic
                          Obj.magic
                          (let v__hd'1658 =
                             Obj.magic
                               Boot.Intrinsics.Mseq.head
                               v__target'1657
                           in
                           Option.Some ()))
                 with
                 | Option.Some () ->
                     (let v_elem'1643 =
                        Obj.magic
                          Boot.Intrinsics.Mseq.get
                          v_field'1639
                          0
                      in
                      let v_slice'1644 =
                        let
                          CRec'1652 ({l1 = v_x'1650})
                        =
                          Obj.magic
                            (Obj.magic
                               Boot.Intrinsics.Mseq.split_at
                               v_field'1639
                               1)
                        in
                        Obj.magic
                          v_x'1650
                      in
                      match
                        Obj.magic
                          (let v__target'1660 =
                             v_field'1640
                           in
                           if
                             Obj.magic
                               Boot.Intrinsics.Mseq.null
                               v__target'1660
                           then
                             Option.None
                           else
                             Obj.magic
                               Obj.magic
                               (let v__hd'1661 =
                                  Obj.magic
                                    Boot.Intrinsics.Mseq.head
                                    v__target'1660
                                in
                                Option.Some ()))
                      with
                      | Option.Some () ->
                          (let v_elem'1645 =
                             Obj.magic
                               Boot.Intrinsics.Mseq.get
                               v_field'1640
                               0
                           in
                           let v_slice'1646 =
                             let
                               CRec'1652 ({l1 = v_x'1648})
                             =
                               Obj.magic
                                 (Obj.magic
                                    Boot.Intrinsics.Mseq.split_at
                                    v_field'1640
                                    1)
                             in
                             Obj.magic
                               v_x'1648
                           in
                           Obj.magic
                             v_matchBody'1638
                             v_elem'1643
                             v_slice'1644
                             v_elem'1645
                             v_slice'1646
                             ())
                      | Option.None ->
                          (Obj.magic
                             false))
                 | Option.None ->
                     (Obj.magic
                        (if
                           Obj.magic
                             Boot.Intrinsics.Mseq.null
                             v_field'1639
                         then
                           if
                             Obj.magic
                               Boot.Intrinsics.Mseq.null
                               v_field'1640
                           then
                             Obj.magic
                               v_matchBody'1637
                               ()
                           else
                             Obj.magic
                               false
                         else
                           Obj.magic
                             false)))
            | Option.None ->
                (Obj.magic
                   (failwith
                      "[No file info] ERROR: Reached a never term, which should be impossible in a well-typed program."))
    in v_work'1470;;
let rec v_forAll =
    fun v_p'1480 ->
      fun v_seq'1481 ->
        if
          Obj.magic
            (Obj.magic
               Boot.Intrinsics.Mseq.null
               v_seq'1481)
        then
          true
        else
          Obj.magic
            (if
               Obj.magic
                 (Obj.magic
                    v_p'1480
                    (Obj.magic
                       Boot.Intrinsics.Mseq.head
                       v_seq'1481))
             then
               Obj.magic
                 v_forAll
                 v_p'1480
                 (Obj.magic
                    Boot.Intrinsics.Mseq.tail
                    v_seq'1481)
             else
               Obj.magic
                 false);;
let v_utestTestPassed =
  fun v_'1482 ->
    Obj.magic
      Boot.Intrinsics.IO.print
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (46) |]);;
let v_utestTestFailed =
  fun v_info'1484 ->
    fun v_lhsStr'1485 ->
      fun v_rhsStr'1486 ->
        fun v_usingStr'1487 ->
          let v_'1488 =
            Obj.magic
              (:=)
              v_numFailed
              (Obj.magic
                 Int.add
                 (Obj.magic
                    (!)
                    v_numFailed)
                 1)
          in
          Obj.magic
            v_printLn
            (Obj.magic
               v_join
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.of_array
                  [| (Obj.magic
                      (Obj.magic
                         Boot.Intrinsics.Mseq.Helpers.of_array
                         [| (10);
                           (32);
                           (42);
                           (42);
                           (32);
                           (85);
                           (110);
                           (105);
                           (116);
                           (32);
                           (116);
                           (101);
                           (115);
                           (116);
                           (32);
                           (70);
                           (65);
                           (73);
                           (76);
                           (69);
                           (68);
                           (58);
                           (32) |]));
                    (Obj.magic
                      v_info'1484);
                    (Obj.magic
                      (Obj.magic
                         Boot.Intrinsics.Mseq.Helpers.of_array
                         [| (42);
                           (42) |]));
                    (Obj.magic
                      (Obj.magic
                         Boot.Intrinsics.Mseq.Helpers.of_array
                         [| (10);
                           (32);
                           (32);
                           (32);
                           (32);
                           (76);
                           (72);
                           (83);
                           (58);
                           (32) |]));
                    (Obj.magic
                      v_lhsStr'1485);
                    (Obj.magic
                      (Obj.magic
                         Boot.Intrinsics.Mseq.Helpers.of_array
                         [| (10);
                           (32);
                           (32);
                           (32);
                           (32);
                           (82);
                           (72);
                           (83);
                           (58);
                           (32) |]));
                    (Obj.magic
                      v_rhsStr'1486);
                    (Obj.magic
                      v_usingStr'1487) |]));;
let v_utestRunner =
  fun v_info'1490 ->
    fun v_usingStr'1491 ->
      fun v_lpprint'1492 ->
        fun v_rpprint'1493 ->
          fun v_eqfunc'1494 ->
            fun v_lhs'1495 ->
              fun v_rhs'1496 ->
                if
                  Obj.magic
                    (Obj.magic
                       v_eqfunc'1494
                       v_lhs'1495
                       v_rhs'1496)
                then
                  Obj.magic
                    v_utestTestPassed
                    ()
                else
                  Obj.magic
                    (Obj.magic
                       v_utestTestFailed
                       v_info'1490
                       (Obj.magic
                          v_lpprint'1492
                          v_lhs'1495)
                       (Obj.magic
                          v_rpprint'1493
                          v_rhs'1496)
                       v_usingStr'1491);;
let rec v_pp'1263 =
    fun v_a'1498 ->
      Obj.magic
        Boot.Intrinsics.Mseq.Helpers.of_array
        [| (63) |]
and v_pp'1261 =
    fun v_a'1499 ->
      Obj.magic
        v_join
        (Obj.magic
           Boot.Intrinsics.Mseq.Helpers.of_array
           [| (Obj.magic
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.of_array
                  [| (91) |]));
             (Obj.magic
               (Obj.magic
                  v_strJoin
                  (Obj.magic
                     Boot.Intrinsics.Mseq.Helpers.of_array
                     [| (44) |])
                  (Obj.magic
                     Boot.Intrinsics.Mseq.map
                     v_pp'1263
                     v_a'1499)));
             (Obj.magic
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.of_array
                  [| (93) |])) |])
and v_pp'1265 =
    fun v_a'1500 ->
      Obj.magic
        v_join
        (Obj.magic
           Boot.Intrinsics.Mseq.Helpers.of_array
           [| (Obj.magic
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.of_array
                  [| (91) |]));
             (Obj.magic
               (Obj.magic
                  v_strJoin
                  (Obj.magic
                     Boot.Intrinsics.Mseq.Helpers.of_array
                     [| (44) |])
                  (Obj.magic
                     Boot.Intrinsics.Mseq.map
                     v_pp'1253
                     v_a'1500)));
             (Obj.magic
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.of_array
                  [| (93) |])) |])
and v_pp'1259 =
    fun v_a'1501 ->
      Obj.magic
        v_join
        (Obj.magic
           Boot.Intrinsics.Mseq.Helpers.of_array
           [| (Obj.magic
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.of_array
                  [| (91) |]));
             (Obj.magic
               (Obj.magic
                  v_strJoin
                  (Obj.magic
                     Boot.Intrinsics.Mseq.Helpers.of_array
                     [| (44) |])
                  (Obj.magic
                     Boot.Intrinsics.Mseq.map
                     v_pp'1255
                     v_a'1501)));
             (Obj.magic
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.of_array
                  [| (93) |])) |])
and v_pp'1253 =
    fun v_a'1502 ->
      Obj.magic
        Boot.Intrinsics.FloatConversion.float2string
        v_a'1502
and v_pp'1255 =
    fun v_a'1503 ->
      Obj.magic
        v_int2string
        v_a'1503
and v_pp =
    fun v_a'1504 ->
      if
        Obj.magic
          v_a'1504
      then
        Obj.magic
          Boot.Intrinsics.Mseq.Helpers.of_array
          [| (116);
            (114);
            (117);
            (101) |]
      else
        Obj.magic
          (Obj.magic
             Boot.Intrinsics.Mseq.Helpers.of_array
             [| (102);
               (97);
               (108);
               (115);
               (101) |]);;
let rec v_eq'1262 =
    fun v_a'1505 ->
      fun v_b'1506 ->
        failwith
          "[No file info] ERROR: Reached a never term, which should be impossible in a well-typed program."
and v_eq'1260 =
    fun v_a'1507 ->
      fun v_b'1508 ->
        Obj.magic
          v_eqSeq
          v_eq'1262
          v_a'1507
          v_b'1508
and v_eq'1264 =
    fun v_a'1509 ->
      fun v_b'1510 ->
        Obj.magic
          v_eqSeq
          v_eq'1252
          v_a'1509
          v_b'1510
and v_eq'1258 =
    fun v_a'1511 ->
      fun v_b'1512 ->
        Obj.magic
          v_eqSeq
          v_eq'1254
          v_a'1511
          v_b'1512
and v_eq'1252 =
    fun v_a'1513 ->
      fun v_b'1514 ->
        Obj.magic
          Float.equal
          v_a'1513
          v_b'1514
and v_eq'1254 =
    fun v_a'1515 ->
      fun v_b'1516 ->
        Obj.magic
          Int.equal
          v_a'1515
          v_b'1516
and v_eq =
    fun v_a'1517 ->
      fun v_b'1518 ->
        if
          Obj.magic
            (if
               Obj.magic
                 v_a'1517
             then
               v_b'1518
             else
               Obj.magic
                 false)
        then
          true
        else
          Obj.magic
            (if
               Obj.magic
                 (if
                    Obj.magic
                      v_a'1517
                  then
                    false
                  else
                    Obj.magic
                      true)
             then
               if
                 Obj.magic
                   v_b'1518
               then
                 false
               else
                 Obj.magic
                   true
             else
               Obj.magic
                 false);;
let v_ =
  let v_maxf'615 =
    fun v_r'613 ->
      fun v_l'614 ->
        if
          Obj.magic
            (Obj.magic
               ((>) : float -> float -> bool)
               v_r'613
               v_l'614)
        then
          v_r'613
        else
          Obj.magic
            v_l'614
  in
  let v_absf'617 =
    fun v_f'616 ->
      Obj.magic
        v_maxf'615
        v_f'616
        (Obj.magic
           Float.neg
           v_f'616)
  in
  let v_eqfApprox'621 =
    fun v_epsilon'618 ->
      fun v_r'619 ->
        fun v_l'620 ->
          if
            Obj.magic
              (Obj.magic
                 ((<=) : float -> float -> bool)
                 (Obj.magic
                    v_absf'617
                    (Obj.magic
                       Float.sub
                       v_r'619
                       v_l'620))
                 v_epsilon'618)
          then
            true
          else
            Obj.magic
              false
  in
  let v_externalExp'622 =
    fun v_x'1663 ->
      (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "exp")))
        v_x'1663
  in
  let v_exp'624 =
    fun v_x'623 ->
      Obj.magic
        v_externalExp'622
        v_x'623
  in
  let v_externalLog'625 =
    fun v_x'1664 ->
      (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "log")))
        v_x'1664
  in
  let v_log'627 =
    fun v_x'626 ->
      Obj.magic
        v_externalLog'625
        v_x'626
  in
  let v_inf'628 =
    Obj.magic
      Float.div
      1.
      0.
  in
  let v_logFactorial'633 =
    fun v_n'629 ->
      let rec v_work'630 =
          fun v_acc'631 ->
            fun v_n'632 ->
              if
                Obj.magic
                  (Obj.magic
                     ((>) : int -> int -> bool)
                     v_n'632
                     0)
              then
                Obj.magic
                  v_work'630
                  (Obj.magic
                     Float.add
                     (Obj.magic
                        v_log'627
                        (Obj.magic
                           float_of_int
                           v_n'632))
                     v_acc'631)
                  (Obj.magic
                     Int.sub
                     v_n'632
                     1)
              else
                Obj.magic
                  v_acc'631
      in Obj.magic
        v_work'630
        0.
        v_n'629
  in
  let v_and'636 =
    fun v_a'634 ->
      fun v_b'635 ->
        if
          Obj.magic
            v_a'634
        then
          v_b'635
        else
          Obj.magic
            false
  in
  let v_externalGammaLogPdf'637 =
    fun v_x'1665 ->
      fun v_x'1666 ->
        fun v_x'1667 ->
          (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalGammaLogPdf")))
            v_x'1665
            ~shape:(v_x'1666)
            ~scale:(v_x'1667)
  in
  let v_externalGammaSample'638 =
    fun v_x'1668 ->
      fun v_x'1669 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalGammaSample")))
          ~shape:(v_x'1668)
          ~scale:(v_x'1669)
  in
  let v_gammaPdf'642 =
    fun v_shape'639 ->
      fun v_scale'640 ->
        fun v_x'641 ->
          Obj.magic
            v_exp'624
            (Obj.magic
               v_externalGammaLogPdf'637
               v_x'641
               v_shape'639
               v_scale'640)
  in
  let v_gammaLogPdf'646 =
    fun v_shape'643 ->
      fun v_scale'644 ->
        fun v_x'645 ->
          Obj.magic
            v_externalGammaLogPdf'637
            v_x'645
            v_shape'643
            v_scale'644
  in
  let v_gammaSample'649 =
    fun v_shape'647 ->
      fun v_scale'648 ->
        Obj.magic
          v_externalGammaSample'638
          v_shape'647
          v_scale'648
  in
  let v_externalBinomialLogPmf'650 =
    fun v_x'1670 ->
      fun v_x'1671 ->
        fun v_x'1672 ->
          (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalBinomialLogPmf")))
            v_x'1670
            ~p:(v_x'1671)
            ~n:(v_x'1672)
  in
  let v_externalBinomialSample'651 =
    fun v_x'1673 ->
      fun v_x'1674 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalBinomialSample")))
          ~p:(v_x'1673)
          ~n:(v_x'1674)
  in
  let v_binomialPmf'655 =
    fun v_p'652 ->
      fun v_n'653 ->
        fun v_x'654 ->
          Obj.magic
            v_exp'624
            (Obj.magic
               v_externalBinomialLogPmf'650
               v_x'654
               v_p'652
               v_n'653)
  in
  let v_binomialLogPmf'659 =
    fun v_p'656 ->
      fun v_n'657 ->
        fun v_x'658 ->
          Obj.magic
            v_externalBinomialLogPmf'650
            v_x'658
            v_p'656
            v_n'657
  in
  let v_binomialSample'662 =
    fun v_p'660 ->
      fun v_n'661 ->
        Obj.magic
          v_externalBinomialSample'651
          v_p'660
          v_n'661
  in
  let v_bernoulliPmf'665 =
    fun v_p'663 ->
      fun v_x'664 ->
        if
          Obj.magic
            v_x'664
        then
          v_p'663
        else
          Obj.magic
            (Obj.magic
               Float.sub
               1.
               v_p'663)
  in
  let v_bernoulliLogPmf'668 =
    fun v_p'666 ->
      fun v_x'667 ->
        Obj.magic
          v_log'627
          (Obj.magic
             v_bernoulliPmf'665
             v_p'666
             v_x'667)
  in
  let v_bernoulliSample'670 =
    fun v_p'669 ->
      if
        Obj.magic
          (Obj.magic
             Int.equal
             1
             (Obj.magic
                v_externalBinomialSample'651
                v_p'669
                1))
      then
        true
      else
        Obj.magic
          false
  in
  let v_externalBetaLogPdf'671 =
    fun v_x'1675 ->
      fun v_x'1676 ->
        fun v_x'1677 ->
          (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalBetaLogPdf")))
            v_x'1675
            ~a:(v_x'1676)
            ~b:(v_x'1677)
  in
  let v_externalBetaSample'672 =
    fun v_x'1678 ->
      fun v_x'1679 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalBetaSample")))
          ~a:(v_x'1678)
          ~b:(v_x'1679)
  in
  let v_betaPdf'676 =
    fun v_a'673 ->
      fun v_b'674 ->
        fun v_x'675 ->
          Obj.magic
            v_exp'624
            (Obj.magic
               v_externalBetaLogPdf'671
               v_x'675
               v_a'673
               v_b'674)
  in
  let v_betaLogPdf'680 =
    fun v_a'677 ->
      fun v_b'678 ->
        fun v_x'679 ->
          Obj.magic
            v_externalBetaLogPdf'671
            v_x'679
            v_a'677
            v_b'678
  in
  let v_betaSample'683 =
    fun v_a'681 ->
      fun v_b'682 ->
        Obj.magic
          v_externalBetaSample'672
          v_a'681
          v_b'682
  in
  let v_externalGaussianLogPdf'684 =
    fun v_x'1680 ->
      fun v_x'1681 ->
        fun v_x'1682 ->
          (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalGaussianLogPdf")))
            v_x'1680
            ~mu:(v_x'1681)
            ~sigma:(v_x'1682)
  in
  let v_externalGaussianSample'685 =
    fun v_x'1683 ->
      fun v_x'1684 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalGaussianSample")))
          ~mu:(v_x'1683)
          ~sigma:(v_x'1684)
  in
  let v_gaussianPdf'689 =
    fun v_mu'686 ->
      fun v_sigma'687 ->
        fun v_x'688 ->
          Obj.magic
            v_exp'624
            (Obj.magic
               v_externalGaussianLogPdf'684
               v_x'688
               v_mu'686
               v_sigma'687)
  in
  let v_gaussianLogPdf'693 =
    fun v_mu'690 ->
      fun v_sigma'691 ->
        fun v_x'692 ->
          Obj.magic
            v_externalGaussianLogPdf'684
            v_x'692
            v_mu'690
            v_sigma'691
  in
  let v_gaussianSample'696 =
    fun v_mu'694 ->
      fun v_sigma'695 ->
        Obj.magic
          v_externalGaussianSample'685
          v_mu'694
          v_sigma'695
  in
  let v_externalMultinomialLogPmf'697 =
    fun v_x'1685 ->
      fun v_x'1687 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalMultinomialLogPmf")))
          (Boot.Intrinsics.Mseq.Helpers.to_array_copy
             v_x'1685)
          ~p:(Boot.Intrinsics.Mseq.Helpers.to_array_copy
            v_x'1687)
  in
  let v_externalMultinomialSample'698 =
    fun v_x'1689 ->
      fun v_x'1690 ->
        Boot.Intrinsics.Mseq.Helpers.of_array_copy
          ((Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalMultinomialSample")))
             v_x'1689
             ~p:(Boot.Intrinsics.Mseq.Helpers.to_array_copy
               v_x'1690))
  in
  let v_externalCategoricalSample'699 =
    fun v_x'1693 ->
      (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalCategoricalSample")))
        (Boot.Intrinsics.Mseq.Helpers.to_array_copy
           v_x'1693)
  in
  let v_multinomialLogPmf'702 =
    fun v_ps'700 ->
      fun v_ns'701 ->
        Obj.magic
          v_externalMultinomialLogPmf'697
          v_ns'701
          v_ps'700
  in
  let v_multinomialPmf'705 =
    fun v_ps'703 ->
      fun v_ns'704 ->
        Obj.magic
          v_exp'624
          (Obj.magic
             v_externalMultinomialLogPmf'697
             v_ns'704
             v_ps'703)
  in
  let v_categoricalLogPmf'708 =
    fun v_ps'706 ->
      fun v_x'707 ->
        Obj.magic
          v_log'627
          (Obj.magic
             Boot.Intrinsics.Mseq.get
             v_ps'706
             v_x'707)
  in
  let v_categoricalPmf'711 =
    fun v_ps'709 ->
      fun v_x'710 ->
        Obj.magic
          Boot.Intrinsics.Mseq.get
          v_ps'709
          v_x'710
  in
  let v_multinomialSample'714 =
    fun v_ps'712 ->
      fun v_n'713 ->
        Obj.magic
          v_externalMultinomialSample'698
          v_n'713
          v_ps'712
  in
  let v_categoricalSample'716 =
    fun v_ps'715 ->
      Obj.magic
        v_externalCategoricalSample'699
        v_ps'715
  in
  let v_externalDirichletLogPdf'717 =
    fun v_x'1695 ->
      fun v_x'1697 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalDirichletLogPdf")))
          (Boot.Intrinsics.Mseq.Helpers.to_array_copy
             v_x'1695)
          ~alpha:(Boot.Intrinsics.Mseq.Helpers.to_array_copy
            v_x'1697)
  in
  let v_externalDirichletSample'718 =
    fun v_x'1699 ->
      Boot.Intrinsics.Mseq.Helpers.of_array_copy
        ((Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalDirichletSample")))
           ~alpha:(Boot.Intrinsics.Mseq.Helpers.to_array_copy
             v_x'1699))
  in
  let v_dirichletLogPdf'721 =
    fun v_alpha'719 ->
      fun v_xs'720 ->
        if
          Obj.magic
            (Obj.magic
               v_eqfApprox'621
               1e-15
               (Obj.magic
                  Boot.Intrinsics.Mseq.Helpers.fold_left
                  Float.add
                  0.
                  v_xs'720)
               1.)
        then
          Obj.magic
            v_externalDirichletLogPdf'717
            v_xs'720
            v_alpha'719
        else
          Obj.magic
            (Obj.magic
               Float.neg
               v_inf'628)
  in
  let v_dirichletPdf'724 =
    fun v_alpha'722 ->
      fun v_xs'723 ->
        Obj.magic
          v_exp'624
          (Obj.magic
             v_externalDirichletLogPdf'717
             v_xs'723
             v_alpha'722)
  in
  let v_dirichletSample'726 =
    fun v_alpha'725 ->
      Obj.magic
        v_externalDirichletSample'718
        v_alpha'725
  in
  let v_externalUniformContinuousSample'727 =
    fun v_x'1702 ->
      fun v_x'1703 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalUniformContinuousSample")))
          v_x'1702
          v_x'1703
  in
  let v_uniformContinuousSample'730 =
    fun v_a'728 ->
      fun v_b'729 ->
        Obj.magic
          v_externalUniformContinuousSample'727
          v_a'728
          v_b'729
  in
  let v_uniformContinuousLogPdf'734 =
    fun v_a'731 ->
      fun v_b'732 ->
        fun v_x'733 ->
          if
            Obj.magic
              (Obj.magic
                 ((>=) : float -> float -> bool)
                 v_x'733
                 v_a'731)
          then
            if
              Obj.magic
                (Obj.magic
                   ((<=) : float -> float -> bool)
                   v_x'733
                   v_b'732)
            then
              Obj.magic
                Float.sub
                (Obj.magic
                   v_log'627
                   1.)
                (Obj.magic
                   v_log'627
                   (Obj.magic
                      Float.sub
                      v_b'732
                      v_a'731))
            else
              Obj.magic
                0.
          else
            Obj.magic
              0.
  in
  let v_uniformContinuousPdf'738 =
    fun v_a'735 ->
      fun v_b'736 ->
        fun v_x'737 ->
          if
            Obj.magic
              (Obj.magic
                 ((>=) : float -> float -> bool)
                 v_x'737
                 v_a'735)
          then
            if
              Obj.magic
                (Obj.magic
                   ((<=) : float -> float -> bool)
                   v_x'737
                   v_b'736)
            then
              Obj.magic
                Float.div
                1.
                (Obj.magic
                   Float.sub
                   v_b'736
                   v_a'735)
            else
              Obj.magic
                0.
          else
            Obj.magic
              0.
  in
  let v_uniformSample'740 =
    fun v_'739 ->
      Obj.magic
        v_uniformContinuousSample'730
        0.
        1.
  in
  let v_externalUniformDiscreteSample'741 =
    fun v_x'1704 ->
      fun v_x'1705 ->
        (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalUniformDiscreteSample")))
          v_x'1704
          v_x'1705
  in
  let v_uniformDiscreteSample'744 =
    fun v_a'742 ->
      fun v_b'743 ->
        Obj.magic
          v_externalUniformDiscreteSample'741
          v_a'742
          v_b'743
  in
  let v_uniformDiscreteLogPdf'748 =
    fun v_a'745 ->
      fun v_b'746 ->
        fun v_x'747 ->
          if
            Obj.magic
              (Obj.magic
                 ((>=) : int -> int -> bool)
                 v_x'747
                 v_a'745)
          then
            if
              Obj.magic
                (Obj.magic
                   ((<=) : int -> int -> bool)
                   v_x'747
                   v_b'746)
            then
              Obj.magic
                Float.sub
                (Obj.magic
                   v_log'627
                   1.)
                (Obj.magic
                   v_log'627
                   (Obj.magic
                      float_of_int
                      (Obj.magic
                         Int.add
                         1
                         (Obj.magic
                            Int.sub
                            v_b'746
                            v_a'745))))
            else
              Obj.magic
                0.
          else
            Obj.magic
              0.
  in
  let v_uniformDiscretePdf'752 =
    fun v_a'749 ->
      fun v_b'750 ->
        fun v_x'751 ->
          if
            Obj.magic
              (Obj.magic
                 ((>=) : int -> int -> bool)
                 v_x'751
                 v_a'749)
          then
            if
              Obj.magic
                (Obj.magic
                   ((<=) : int -> int -> bool)
                   v_x'751
                   v_b'750)
            then
              Obj.magic
                Float.div
                1.
                (Obj.magic
                   float_of_int
                   (Obj.magic
                      Int.add
                      1
                      (Obj.magic
                         Int.sub
                         v_b'750
                         v_a'749)))
            else
              Obj.magic
                0.
          else
            Obj.magic
              0.
  in
  let v_poissonLogPmf'755 =
    fun v_lambda'753 ->
      fun v_x'754 ->
        Obj.magic
          Float.sub
          (Obj.magic
             Float.sub
             (Obj.magic
                Float.mul
                (Obj.magic
                   float_of_int
                   v_x'754)
                (Obj.magic
                   v_log'627
                   v_lambda'753))
             v_lambda'753)
          (Obj.magic
             v_logFactorial'633
             v_x'754)
  in
  let v_poissonPmf'758 =
    fun v_lambda'756 ->
      fun v_x'757 ->
        Obj.magic
          v_exp'624
          (Obj.magic
             v_poissonLogPmf'755
             v_lambda'756
             v_x'757)
  in
  let v_poissonSample'768 =
    fun v_lambda'759 ->
      let v_enlam'760 =
        Obj.magic
          v_exp'624
          (Obj.magic
             Float.neg
             v_lambda'759)
      in
      let v_x'761 =
        0
      in
      let v_prod'762 =
        1.
      in
      let rec v_rec'763 =
          fun v_x'764 ->
            fun v_prod'765 ->
              let v_u'766 =
                Obj.magic
                  v_uniformSample'740
                  ()
              in
              let v_prod'767 =
                Obj.magic
                  Float.mul
                  v_prod'765
                  v_u'766
              in
              if
                Obj.magic
                  (Obj.magic
                     ((>) : float -> float -> bool)
                     v_prod'767
                     v_enlam'760)
              then
                Obj.magic
                  v_rec'763
                  (Obj.magic
                     Int.add
                     v_x'764
                     1)
                  v_prod'767
              else
                Obj.magic
                  v_x'764
      in Obj.magic
        v_rec'763
        v_x'761
        v_prod'762
  in
  let v_externalExponentialSample'769 =
    fun v_x'1706 ->
      (Boot.Intrinsics.Ext.get_external (Boot.Intrinsics.Mseq.Helpers.of_ustring (Boot.Ustring.from_utf8 "externalExponentialSample")))
        ~lambda:(v_x'1706)
  in
  let v_exponentialSample'771 =
    fun v_lambda'770 ->
      Obj.magic
        v_externalExponentialSample'769
        v_lambda'770
  in
  let v_exponentialLogPdf'774 =
    fun v_lambda'772 ->
      fun v_x'773 ->
        Obj.magic
          Float.sub
          (Obj.magic
             v_log'627
             v_lambda'772)
          (Obj.magic
             Float.mul
             v_lambda'772
             v_x'773)
  in
  let v_exponentialPdf'777 =
    fun v_lambda'775 ->
      fun v_x'776 ->
        Obj.magic
          v_exp'624
          (Obj.magic
             v_exponentialLogPdf'774
             v_lambda'775
             v_x'776)
  in
  let v_maxf'780 =
    fun v_r'778 ->
      fun v_l'779 ->
        if
          Obj.magic
            (Obj.magic
               ((>) : float -> float -> bool)
               v_r'778
               v_l'779)
        then
          v_r'778
        else
          Obj.magic
            v_l'779
  in
  let v_absf'782 =
    fun v_f'781 ->
      Obj.magic
        v_maxf'780
        v_f'781
        (Obj.magic
           Float.neg
           v_f'781)
  in
  let v_eqfApprox'786 =
    fun v_epsilon'783 ->
      fun v_r'784 ->
        fun v_l'785 ->
          if
            Obj.magic
              (Obj.magic
                 ((<=) : float -> float -> bool)
                 (Obj.magic
                    v_absf'782
                    (Obj.magic
                       Float.sub
                       v_r'784
                       v_l'785))
                 v_epsilon'783)
          then
            true
          else
            Obj.magic
              false
  in
  let v__eqf'787 =
    Obj.magic
      v_eqfApprox'786
      1e-11
  in
  let v_intRange'792 =
    fun v_lower'788 ->
      fun v_upper'789 ->
        fun v_r'790 ->
          fun v_l'791 ->
            Obj.magic
              v_and'636
              (Obj.magic
                 v_and'636
                 (Obj.magic
                    ((<=) : int -> int -> bool)
                    v_r'790
                    v_upper'789)
                 (Obj.magic
                    ((>=) : int -> int -> bool)
                    v_r'790
                    v_lower'788))
              (Obj.magic
                 v_and'636
                 (Obj.magic
                    ((<=) : int -> int -> bool)
                    v_l'791
                    v_upper'789)
                 (Obj.magic
                    ((>=) : int -> int -> bool)
                    v_l'791
                    v_lower'788))
  in
  let v_floatRange'797 =
    fun v_lower'793 ->
      fun v_upper'794 ->
        fun v_r'795 ->
          fun v_l'796 ->
            Obj.magic
              v_and'636
              (Obj.magic
                 v_and'636
                 (Obj.magic
                    ((<=) : float -> float -> bool)
                    v_r'795
                    v_upper'794)
                 (Obj.magic
                    ((>=) : float -> float -> bool)
                    v_r'795
                    v_lower'793))
              (Obj.magic
                 v_and'636
                 (Obj.magic
                    ((<=) : float -> float -> bool)
                    v_l'796
                    v_upper'794)
                 (Obj.magic
                    ((>=) : float -> float -> bool)
                    v_l'796
                    v_lower'793))
  in
  let v_'1521 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (53);
           (53);
           (58);
           (48);
           (45);
           (49);
           (53);
           (53);
           (58);
           (53);
           (52);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1519 ->
         fun v_b'1520 ->
           Obj.magic
             v__eqf'787
             v_a'1519
             v_b'1520)
      (Obj.magic
         v_gammaPdf'642
         1.
         2.
         1.)
      0.303265329856
  in
  let v_'1524 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (53);
           (54);
           (58);
           (48);
           (45);
           (49);
           (53);
           (54);
           (58);
           (54);
           (52);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1522 ->
         fun v_b'1523 ->
           Obj.magic
             v__eqf'787
             v_a'1522
             v_b'1523)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_gammaLogPdf'646
            2.
            3.
            1.))
      0.0796145900638
  in
  let v_'1527 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (53);
           (55);
           (58);
           (48);
           (45);
           (49);
           (53);
           (55);
           (58);
           (53);
           (53);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (102);
           (108);
           (111);
           (97);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (46);
           (10);
           (32);
           (32);
           (105);
           (110);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1525 ->
         fun v_b'1526 ->
           Obj.magic
             v_floatRange'797
             0.
             v_inf'628
             v_a'1525
             v_b'1526)
      (Obj.magic
         v_gammaSample'649
         1.
         2.)
      1.
  in
  let v_'1530 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (48);
           (58);
           (48);
           (45);
           (49);
           (54);
           (48);
           (58);
           (53);
           (55);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1528 ->
         fun v_b'1529 ->
           Obj.magic
             v__eqf'787
             v_a'1528
             v_b'1529)
      (Obj.magic
         v_binomialPmf'655
         0.7
         20
         15)
      0.17886305057
  in
  let v_'1533 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (49);
           (58);
           (48);
           (45);
           (49);
           (54);
           (49);
           (58);
           (54);
           (54);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1531 ->
         fun v_b'1532 ->
           Obj.magic
             v__eqf'787
             v_a'1531
             v_b'1532)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_binomialLogPmf'659
            0.5
            40
            20))
      0.12537068762
  in
  let v_'1536 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (50);
           (58);
           (48);
           (45);
           (49);
           (54);
           (50);
           (58);
           (53);
           (52);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (105);
           (110);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (10);
           (32);
           (32);
           (50);
           (48) |])
      v_pp'1255
      v_pp'1255
      (fun v_a'1534 ->
         fun v_b'1535 ->
           Obj.magic
             v_intRange'792
             0
             20
             v_a'1534
             v_b'1535)
      (Obj.magic
         v_binomialSample'662
         0.7
         20)
      0
  in
  let v_'1539 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (51);
           (58);
           (48);
           (45);
           (49);
           (54);
           (51);
           (58);
           (52);
           (56);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1537 ->
         fun v_b'1538 ->
           Obj.magic
             v__eqf'787
             v_a'1537
             v_b'1538)
      (Obj.magic
         v_bernoulliPmf'665
         0.3
         false)
      0.7
  in
  let v_'1542 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (52);
           (58);
           (48);
           (45);
           (49);
           (54);
           (52);
           (58);
           (53);
           (54);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1540 ->
         fun v_b'1541 ->
           Obj.magic
             v__eqf'787
             v_a'1540
             v_b'1541)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_bernoulliLogPmf'668
            0.6
            true))
      0.6
  in
  let v_'1545 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (53);
           (58);
           (48);
           (45);
           (49);
           (54);
           (53);
           (58);
           (53);
           (55);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (108);
           (97);
           (109);
           (32);
           (35);
           (118);
           (97);
           (114);
           (34);
           (34);
           (58);
           (32);
           (66);
           (111);
           (111);
           (108);
           (46);
           (10);
           (32);
           (32);
           (108);
           (97);
           (109);
           (32);
           (35);
           (118);
           (97);
           (114);
           (34);
           (49);
           (34);
           (58);
           (32);
           (66);
           (111);
           (111);
           (108);
           (46);
           (10);
           (32);
           (32);
           (32);
           (32);
           (116);
           (114);
           (117);
           (101) |])
      v_pp
      v_pp
      (fun v_a'1543 ->
         fun v_b'1544 ->
           Obj.magic
             (fun v_'806 ->
                fun v_'807 ->
                  true)
             v_a'1543
             v_b'1544)
      (Obj.magic
         v_bernoulliSample'670
         0.6)
      false
  in
  let v_'1548 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (56);
           (58);
           (48);
           (45);
           (49);
           (54);
           (56);
           (58);
           (52);
           (51);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1546 ->
         fun v_b'1547 ->
           Obj.magic
             v__eqf'787
             v_a'1546
             v_b'1547)
      (Obj.magic
         v_betaPdf'676
         2.
         2.
         0.5)
      1.5
  in
  let v_'1551 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (54);
           (57);
           (58);
           (48);
           (45);
           (49);
           (54);
           (57);
           (58);
           (53);
           (53);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1549 ->
         fun v_b'1550 ->
           Obj.magic
             v__eqf'787
             v_a'1549
             v_b'1550)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_betaLogPdf'680
            2.
            5.
            0.2))
      2.4576
  in
  let v_'1554 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (55);
           (48);
           (58);
           (48);
           (45);
           (49);
           (55);
           (48);
           (58);
           (53);
           (51);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (102);
           (108);
           (111);
           (97);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (46);
           (10);
           (32);
           (32);
           (49);
           (46) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1552 ->
         fun v_b'1553 ->
           Obj.magic
             v_floatRange'797
             0.
             1.
             v_a'1552
             v_b'1553)
      (Obj.magic
         v_betaSample'683
         2.
         2.)
      0.
  in
  let v_'1557 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (55);
           (51);
           (58);
           (48);
           (45);
           (49);
           (55);
           (51);
           (58);
           (53);
           (56);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1555 ->
         fun v_b'1556 ->
           Obj.magic
             v__eqf'787
             v_a'1555
             v_b'1556)
      (Obj.magic
         v_gaussianPdf'689
         0.
         0.4472
         0.)
      0.892089178
  in
  let v_'1560 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (55);
           (52);
           (58);
           (48);
           (45);
           (49);
           (55);
           (52);
           (58);
           (54);
           (54);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1558 ->
         fun v_b'1559 ->
           Obj.magic
             v__eqf'787
             v_a'1558
             v_b'1559)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_gaussianLogPdf'693
            2.
            1.
            2.))
      0.398942280401
  in
  let v_'1563 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (55);
           (53);
           (58);
           (48);
           (45);
           (49);
           (55);
           (53);
           (58);
           (53);
           (54);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (108);
           (97);
           (109);
           (32);
           (35);
           (118);
           (97);
           (114);
           (34);
           (34);
           (58);
           (32);
           (70);
           (108);
           (111);
           (97);
           (116);
           (46);
           (10);
           (32);
           (32);
           (108);
           (97);
           (109);
           (32);
           (35);
           (118);
           (97);
           (114);
           (34);
           (49);
           (34);
           (58);
           (32);
           (70);
           (108);
           (111);
           (97);
           (116);
           (46);
           (10);
           (32);
           (32);
           (32);
           (32);
           (116);
           (114);
           (117);
           (101) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1561 ->
         fun v_b'1562 ->
           Obj.magic
             (fun v_'804 ->
                fun v_'805 ->
                  true)
             v_a'1561
             v_b'1562)
      (Obj.magic
         v_gaussianSample'696
         0.
         0.2)
      0.
  in
  let v_'1566 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (55);
           (56);
           (58);
           (48);
           (45);
           (49);
           (55);
           (56);
           (58);
           (55);
           (49);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1564 ->
         fun v_b'1565 ->
           Obj.magic
             v__eqf'787
             v_a'1564
             v_b'1565)
      (Obj.magic
         v_multinomialLogPmf'702
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.1);
              (0.3);
              (0.6) |])
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0);
              (1);
              (0) |]))
      (Obj.magic
         v_log'627
         0.3)
  in
  let v_'1569 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (55);
           (57);
           (58);
           (48);
           (45);
           (49);
           (55);
           (57);
           (58);
           (54);
           (52);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1567 ->
         fun v_b'1568 ->
           Obj.magic
             v__eqf'787
             v_a'1567
             v_b'1568)
      (Obj.magic
         v_multinomialPmf'705
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.1);
              (0.3);
              (0.6) |])
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0);
              (0);
              (1) |]))
      0.6
  in
  let v_'1572 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (56);
           (48);
           (58);
           (48);
           (45);
           (49);
           (56);
           (48);
           (58);
           (54);
           (55);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1570 ->
         fun v_b'1571 ->
           Obj.magic
             v__eqf'787
             v_a'1570
             v_b'1571)
      (Obj.magic
         v_multinomialPmf'705
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.1);
              (0.3);
              (0.6) |])
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0);
              (2);
              (3) |]))
      0.1944
  in
  let v_'1575 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (56);
           (49);
           (58);
           (48);
           (45);
           (49);
           (56);
           (49);
           (58);
           (54);
           (53);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1573 ->
         fun v_b'1574 ->
           Obj.magic
             v__eqf'787
             v_a'1573
             v_b'1574)
      (Obj.magic
         v_categoricalLogPmf'708
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.3);
              (0.2);
              (0.5) |])
         2)
      (Obj.magic
         v_log'627
         0.5)
  in
  let v_'1578 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (56);
           (50);
           (58);
           (48);
           (45);
           (49);
           (56);
           (50);
           (58);
           (53);
           (56);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1576 ->
         fun v_b'1577 ->
           Obj.magic
             v__eqf'787
             v_a'1576
             v_b'1577)
      (Obj.magic
         v_categoricalPmf'711
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.1);
              (0.3);
              (0.6) |])
         2)
      0.6
  in
  let v_'1581 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (56);
           (51);
           (58);
           (48);
           (45);
           (49);
           (56);
           (51);
           (58);
           (53);
           (56);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1579 ->
         fun v_b'1580 ->
           Obj.magic
             v__eqf'787
             v_a'1579
             v_b'1580)
      (Obj.magic
         v_categoricalPmf'711
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.1);
              (0.3);
              (0.6) |])
         1)
      0.3
  in
  let v_'1584 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (56);
           (52);
           (58);
           (48);
           (45);
           (49);
           (56);
           (53);
           (58);
           (55);
           (49);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (108);
           (97);
           (109);
           (32);
           (108);
           (58);
           (32);
           (91);
           (73);
           (110);
           (116);
           (93);
           (46);
           (10);
           (32);
           (32);
           (108);
           (97);
           (109);
           (32);
           (114);
           (58);
           (32);
           (91);
           (97);
           (93);
           (46);
           (10);
           (32);
           (32);
           (32);
           (32);
           (109);
           (97);
           (116);
           (99);
           (104);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (108);
           (10);
           (32);
           (32);
           (32);
           (32);
           (119);
           (105);
           (116);
           (104);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (91);
           (32);
           (118);
           (49);
           (44);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (118);
           (50);
           (32);
           (93);
           (10);
           (32);
           (32);
           (32);
           (32);
           (116);
           (104);
           (101);
           (110);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (101);
           (113);
           (105);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (40);
           (97);
           (100);
           (100);
           (105);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (118);
           (49);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (118);
           (50);
           (41);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (51);
           (10);
           (32);
           (32);
           (32);
           (32);
           (101);
           (108);
           (115);
           (101);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (102);
           (97);
           (108);
           (115);
           (101) |])
      v_pp'1259
      v_pp'1261
      (fun v_a'1582 ->
         fun v_b'1583 ->
           Obj.magic
             (fun v_l'800 ->
                fun v_r'801 ->
                  match
                    Obj.magic
                      (let v__target'1707 =
                         v_l'800
                       in
                       if
                         Obj.magic
                           Int.equal
                           (Obj.magic
                              Boot.Intrinsics.Mseq.length
                              v__target'1707)
                           2
                       then
                         let v__seqElem'1708 =
                           Obj.magic
                             Boot.Intrinsics.Mseq.get
                             v__target'1707
                             0
                         in
                         let v__seqElem'1709 =
                           Obj.magic
                             Boot.Intrinsics.Mseq.get
                             v__target'1707
                             1
                         in
                         Option.Some (v__seqElem'1708, v__seqElem'1709)
                       else
                         Obj.magic
                           Obj.magic
                           (Option.None))
                  with
                  | Option.Some (v_v1'802, v_v2'803) ->
                      (Obj.magic
                         Int.equal
                         (Obj.magic
                            Int.add
                            v_v1'802
                            v_v2'803)
                         3)
                  | Option.None ->
                      (Obj.magic
                         false))
             v_a'1582
             v_b'1583)
      (Obj.magic
         v_multinomialSample'714
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.2);
              (0.8) |])
         3)
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [|  |])
  in
  let v_'1587 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (56);
           (54);
           (58);
           (48);
           (45);
           (49);
           (56);
           (54);
           (58);
           (55);
           (48);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (105);
           (110);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (10);
           (32);
           (32);
           (51) |])
      v_pp'1255
      v_pp'1255
      (fun v_a'1585 ->
         fun v_b'1586 ->
           Obj.magic
             v_intRange'792
             0
             3
             v_a'1585
             v_b'1586)
      (Obj.magic
         v_categoricalSample'716
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.1);
              (0.4);
              (0.2);
              (0.3) |]))
      0
  in
  let v_'1590 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (56);
           (57);
           (58);
           (48);
           (45);
           (49);
           (57);
           (48);
           (58);
           (51);
           (49);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1588 ->
         fun v_b'1589 ->
           Obj.magic
             v__eqf'787
             v_a'1588
             v_b'1589)
      (Obj.magic
         v_dirichletLogPdf'721
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (1.5);
              (1.5);
              (1.5) |])
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.5);
              (0.25);
              (0.25) |]))
      1.08321533235
  in
  let v_'1593 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (57);
           (49);
           (58);
           (48);
           (45);
           (49);
           (57);
           (49);
           (58);
           (55);
           (52);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1591 ->
         fun v_b'1592 ->
           Obj.magic
             v__eqf'787
             v_a'1591
             v_b'1592)
      (Obj.magic
         v_dirichletPdf'724
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (1.);
              (1.);
              (2.) |])
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (0.01);
              (0.01);
              (0.98) |]))
      5.88
  in
  let v_'1596 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (57);
           (50);
           (58);
           (48);
           (45);
           (49);
           (57);
           (51);
           (58);
           (52);
           (50);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (108);
           (97);
           (109);
           (32);
           (108);
           (58);
           (32);
           (91);
           (70);
           (108);
           (111);
           (97);
           (116);
           (93);
           (46);
           (10);
           (32);
           (32);
           (108);
           (97);
           (109);
           (32);
           (114);
           (58);
           (32);
           (91);
           (70);
           (108);
           (111);
           (97);
           (116);
           (93);
           (46);
           (10);
           (32);
           (32);
           (32);
           (32);
           (95);
           (101);
           (113);
           (102);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (40);
           (102);
           (111);
           (108);
           (100);
           (108);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (97);
           (100);
           (100);
           (102);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (48);
           (46);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (108);
           (41);
           (10);
           (32);
           (32);
           (32);
           (32);
           (32);
           (32);
           (49);
           (46) |])
      v_pp'1265
      v_pp'1265
      (fun v_a'1594 ->
         fun v_b'1595 ->
           Obj.magic
             (fun v_l'798 ->
                fun v_r'799 ->
                  Obj.magic
                    v__eqf'787
                    (Obj.magic
                       Boot.Intrinsics.Mseq.Helpers.fold_left
                       Float.add
                       0.
                       v_l'798)
                    1.)
             v_a'1594
             v_b'1595)
      (Obj.magic
         v_dirichletSample'726
         (Obj.magic
            Boot.Intrinsics.Mseq.Helpers.of_array
            [| (5.);
              (5.);
              (5.) |]))
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (0.) |])
  in
  let v_'1599 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (57);
           (54);
           (58);
           (48);
           (45);
           (49);
           (57);
           (54);
           (58);
           (54);
           (56);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (102);
           (108);
           (111);
           (97);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (46);
           (10);
           (32);
           (32);
           (50);
           (46) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1597 ->
         fun v_b'1598 ->
           Obj.magic
             v_floatRange'797
             0.
             2.
             v_a'1597
             v_b'1598)
      (Obj.magic
         v_uniformContinuousSample'730
         1.
         2.)
      0.
  in
  let v_'1602 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (57);
           (55);
           (58);
           (48);
           (45);
           (49);
           (57);
           (55);
           (58);
           (54);
           (55);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1600 ->
         fun v_b'1601 ->
           Obj.magic
             v__eqf'787
             v_a'1600
             v_b'1601)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_uniformContinuousLogPdf'734
            1.
            2.
            1.5))
      1.
  in
  let v_'1605 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (49);
           (57);
           (56);
           (58);
           (48);
           (45);
           (49);
           (57);
           (56);
           (58);
           (53);
           (56);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1603 ->
         fun v_b'1604 ->
           Obj.magic
             v__eqf'787
             v_a'1603
             v_b'1604)
      (Obj.magic
         v_uniformContinuousPdf'738
         1.
         2.
         1.5)
      1.
  in
  let v_'1608 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (48);
           (49);
           (58);
           (48);
           (45);
           (50);
           (48);
           (49);
           (58);
           (53);
           (51);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (102);
           (108);
           (111);
           (97);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (46);
           (10);
           (32);
           (32);
           (49);
           (46) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1606 ->
         fun v_b'1607 ->
           Obj.magic
             v_floatRange'797
             0.
             1.
             v_a'1606
             v_b'1607)
      (Obj.magic
         v_uniformSample'740
         ())
      0.
  in
  let v_'1611 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (48);
           (52);
           (58);
           (48);
           (45);
           (50);
           (48);
           (52);
           (58);
           (53);
           (55);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (105);
           (110);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (51);
           (10);
           (32);
           (32);
           (56) |])
      v_pp'1255
      v_pp'1255
      (fun v_a'1609 ->
         fun v_b'1610 ->
           Obj.magic
             v_intRange'792
             3
             8
             v_a'1609
             v_b'1610)
      (Obj.magic
         v_uniformDiscreteSample'744
         3
         8)
      3
  in
  let v_'1614 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (48);
           (53);
           (58);
           (48);
           (45);
           (50);
           (48);
           (53);
           (58);
           (53);
           (57);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1612 ->
         fun v_b'1613 ->
           Obj.magic
             v__eqf'787
             v_a'1612
             v_b'1613)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_uniformDiscreteLogPdf'748
            1
            2
            1))
      0.5
  in
  let v_'1617 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (48);
           (54);
           (58);
           (48);
           (45);
           (50);
           (48);
           (54);
           (58);
           (53);
           (48);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1615 ->
         fun v_b'1616 ->
           Obj.magic
             v__eqf'787
             v_a'1615
             v_b'1616)
      (Obj.magic
         v_uniformDiscretePdf'752
         1
         2
         1)
      0.5
  in
  let v_'1620 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (48);
           (57);
           (58);
           (48);
           (45);
           (50);
           (48);
           (57);
           (58);
           (53);
           (51);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1618 ->
         fun v_b'1619 ->
           Obj.magic
             v__eqf'787
             v_a'1618
             v_b'1619)
      (Obj.magic
         v_poissonPmf'758
         2.
         2)
      0.270670566473
  in
  let v_'1623 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (49);
           (48);
           (58);
           (48);
           (45);
           (50);
           (49);
           (48);
           (58);
           (54);
           (50);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1621 ->
         fun v_b'1622 ->
           Obj.magic
             v__eqf'787
             v_a'1621
             v_b'1622)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_poissonLogPmf'755
            3.
            2))
      0.224041807655
  in
  let v_'1626 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (49);
           (49);
           (58);
           (48);
           (45);
           (50);
           (49);
           (49);
           (58);
           (53);
           (52);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (105);
           (110);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (10);
           (32);
           (32);
           (49);
           (48);
           (48);
           (48);
           (48);
           (48) |])
      v_pp'1255
      v_pp'1255
      (fun v_a'1624 ->
         fun v_b'1625 ->
           Obj.magic
             v_intRange'792
             0
             100000
             v_a'1624
             v_b'1625)
      (Obj.magic
         v_poissonSample'768
         2.)
      3
  in
  let v_'1629 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (49);
           (52);
           (58);
           (48);
           (45);
           (50);
           (49);
           (52);
           (58);
           (53);
           (57);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (102);
           (108);
           (111);
           (97);
           (116);
           (82);
           (97);
           (110);
           (103);
           (101);
           (10);
           (32);
           (32);
           (48);
           (46);
           (10);
           (32);
           (32);
           (105);
           (110);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1627 ->
         fun v_b'1628 ->
           Obj.magic
             v_floatRange'797
             0.
             v_inf'628
             v_a'1627
             v_b'1628)
      (Obj.magic
         v_exponentialSample'771
         1.)
      0.
  in
  let v_'1632 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (49);
           (53);
           (58);
           (48);
           (45);
           (50);
           (49);
           (53);
           (58);
           (54);
           (56);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1630 ->
         fun v_b'1631 ->
           Obj.magic
             v__eqf'787
             v_a'1630
             v_b'1631)
      (Obj.magic
         v_exp'624
         (Obj.magic
            v_exponentialLogPdf'774
            1.
            2.))
      0.135335283237
  in
  let v_'1635 =
    Obj.magic
      v_utestRunner
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (70);
           (73);
           (76);
           (69);
           (32);
           (34);
           (47);
           (104);
           (111);
           (109);
           (101);
           (47);
           (108);
           (97);
           (114);
           (115);
           (47);
           (68);
           (111);
           (99);
           (117);
           (109);
           (101);
           (110);
           (116);
           (115);
           (47);
           (77);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (109);
           (105);
           (107);
           (105);
           (110);
           (103);
           (47);
           (115);
           (116);
           (100);
           (108);
           (105);
           (98);
           (47);
           (101);
           (120);
           (116);
           (47);
           (100);
           (105);
           (115);
           (116);
           (45);
           (101);
           (120);
           (116);
           (46);
           (109);
           (99);
           (34);
           (32);
           (50);
           (49);
           (54);
           (58);
           (48);
           (45);
           (50);
           (49);
           (54);
           (58);
           (54);
           (48);
           (32) |])
      (Obj.magic
         Boot.Intrinsics.Mseq.Helpers.of_array
         [| (10);
           (32);
           (32);
           (32);
           (32);
           (85);
           (115);
           (105);
           (110);
           (103);
           (58);
           (32);
           (95);
           (101);
           (113);
           (102) |])
      v_pp'1253
      v_pp'1253
      (fun v_a'1633 ->
         fun v_b'1634 ->
           Obj.magic
             v__eqf'787
             v_a'1633
             v_b'1634)
      (Obj.magic
         v_exponentialPdf'777
         2.
         2.)
      0.0366312777775
  in
  if
    Obj.magic
      (Obj.magic
         ((>) : int -> int -> bool)
         (Obj.magic
            (!)
            v_numFailed)
         0)
  then
    let v_'1455 =
      ()
    in
    Obj.magic
      Boot.Intrinsics.MSys.exit
      1
  else
    Obj.magic
      ();;
Obj.magic
  Boot.Intrinsics.IO.print
  (Obj.magic
     Boot.Intrinsics.Mseq.Helpers.of_array
     [| (10) |]);;
