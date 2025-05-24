(* score/Scoring.sml : 1.1-118.1 *)
(* molasses-file19.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
(**************************************************************************)
(* Scoring (lab dependent)                                                *)
(**************************************************************************)
structure Score =
  struct
    fun failure () : unit =
      if not (! Config.autograder) then
        ()
      else
        JSONPrinter.print (TextIO.stdOut, JSON.NULL)

    fun compiler (stats : 'a Stats.result) : unit =
      if not (! Config.autograder) then
        ()
      else
        let
          fun stat1 suite ((_, _, file), (cat, res)) =
            let
              val {base, ext} = OS.Path.splitBaseExt file
              val ext =
                case ext of
                  NONE => raise Fail ("Test with bad/no extension: " ^ file)
                | SOME ext =>
                    if size ext = 2 then
                      ext
                    else
                      failwithf
                        "File %s has an extension that is not 2 characters long"
                        [string file]
            in
              JSON.OBJECT
                [ ("suite", JSON.STRING suite)
                , ("name", JSON.STRING base)
                , ("lang", JSON.STRING ext)
                , ("cat", JSON.STRING (Summary.catToString cat))
                , ("res", JSON.STRING (Summary.resToString res))
                ]
            end

          fun statsuite {suite, goodTests, badTests = _} =
            map (stat1 suite) goodTests

          val (passed, didNotPass) =
            List.partition (fn (_, (_, res)) => res = Summary.SUCC)
              (concatMap # goodTests stats)

          val (timeout, failed) =
            List.partition (fn (_, (_, res)) => res = Summary.TIME) didNotPass
        in
          JSONPrinter.print
            ( TextIO.stdOut
            , JSON.OBJECT
                [ ("passed", JSON.INT (IntInf.fromInt (length passed)))
                , ("failed", JSON.INT (IntInf.fromInt (length failed)))
                , ("timeout", JSON.INT (IntInf.fromInt (length timeout)))
                ]
            )
        end

    fun benchmarks stats =
      if not (! Config.autograder) then
        ()
      else
        let
          val v =
            concatMap (fn {badTests, goodTests} => badTests @ goodTests) stats
          val success = List.all (fn (_, JSON.NULL) => false | _ => true) v
        in
          JSONPrinter.print' {strm = TextIO.stdOut, pretty = false}
            (JSON.OBJECT
               [ ("compiles", JSON.BOOL true)
               , ("success", JSON.BOOL success)
               , ("results", JSON.OBJECT v)
               ])
        end

    fun tests (stats : Compiler.lib Stats.result) =
      let
        val allInvalidTests = concatMap # badTests stats
        val allValidTests = concatMap # goodTests stats

        (* Valid tests are partitioned into good and bad *)
        val (allGoodTests, allBadTests) =
          List.partition (fn (_, (_, res)) => res = Summary.SUCC) allValidTests

        (* For good tests, we want individual statistics *)
        fun catSize f =
          let
            val listWithCat =
              List.filter (fn (_, (cat, _)) => f cat) allGoodTests
          in
            length listWithCat
          end

        val INT' = JSON.INT o IntInf.fromInt
      in
        JSON.OBJECT
          [ ("invalid", INT' (length allInvalidTests))
          , ("good", INT' (length allGoodTests))
          , ("bad", INT' (length allBadTests))
          , ("err", INT' (catSize (fn cat => cat = Summary.ERR)))
          , ( "ret"
            , INT'
                (catSize
                   (fn cat =>
                      cat = Summary.RET orelse cat = Summary.OR_LOOP Summary.RET))
            )
          , ( "raise"
            , INT'
                (catSize
                   (fn cat =>
                      cat = Summary.RAISE orelse cat = Summary.OR_LOOP
                                                         Summary.RAISE))
            )
          , ( "header"
            , INT' <| length
                        (List.filter
                           (fn ((_, Compiler.HEADER _, _), _) => true
                             | _ => false)
                           allGoodTests)
            )
          ]
      end
  end

end
