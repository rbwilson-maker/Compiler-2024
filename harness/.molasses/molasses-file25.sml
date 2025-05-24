(* grader/Bench.sml : 1.1-75.1 *)
(* molasses-file25.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
structure Bench =
  struct
    (* Benchmark a single file in suiteDir *)
    fun one (suiteDir, lib, file) =
      let
        val test = suiteDir // file

        fun desc (optimize, safe) =
          (if safe then
             "            -O"
           else
             "   --unsafe -O")
          ^ Int.toString optimize
          ^ ": "

        (* Wrap calls to Compiler.benchmark in reporting code *)
        fun benchmark (test, optimize, safe) =
          let val res = Compiler.benchmark (test, optimize, safe) in
            case res of
              Outcome.TIME_OK (cycles, codesize) =>
                ( printq 1
                    (Util.green (desc (optimize, safe) ^ IntInf.toString cycles))
                ; printq 1
                    (Util.green
                       ("            Executable size in bytes:" ^ IntInf.toString
                                                                    codesize))
                ; SOME res
                )
            | Outcome.TIME_INF =>
                ( printq 4
                    (Util.yellow
                       (desc (optimize, safe)
                        ^ "Cycle counting failed: "
                        ^ Outcome.toString (Outcome.INFLOOP (SOME Outcome.ALRM))))
                ; SOME res
                )
          (* But, it's still ok. *)
          end
            handle
                Fail msg =>
                  ( printq 4
                      (Util.red (desc (optimize, safe) ^ "Failed: " ^ msg))
                  ; NONE
                  )

      (* Disallow errors, but allow timeouts as long as _something_ passed. *)
      (*      fun cycMin (NONE,  _) = NONE
              | cycMin (_ , NONE) = NONE
              | cycMin (ok, SOME (Outcome.TIME_INF)) = ok
              | cycMin (SOME (Outcome.TIME_OK (t1, s2)), SOME (Outcome.TIME_OK (t2, s2))) =
                        SOME (Outcome.TIME_OK (IntInf.min (t1,t2), IntInf.min (s1,s2)))*)

      (* for now, we don't run -O0 since they are not part of the grading function *)
      in
        case Outcome.readTestDirective test of
          SOME (Outcome.RETURN v) =>
            let
              val () = printq 2 ""
              val () = printq 5 (Util.bold ("-- Timing file " ^ test ^ " --"))
              (*val safe0   = benchmark (test, 0, true)
              val unsafe0 = benchmark (test, 0, false)*)
              val safe1 = benchmark (test, 1, true)
              val unsafe1 = benchmark (test, 1, false)
            (*val msafe   = List.foldl cycMin (SOME Outcome.TIME_INF) [ safe0, safe1 ]
            val munsafe = List.foldl cycMin (SOME Outcome.TIME_INF) [ unsafe0, unsafe1 ]*)
            in
              case (safe1, unsafe1) of
                ( SOME (Outcome.TIME_OK (t1, s1))
                , SOME (Outcome.TIME_OK (t2, s2))
                ) =>
                  SOME
                    (JSON.ARRAY
                       [ JSON.ARRAY [JSON.INT t1, JSON.INT t2]
                       , JSON.ARRAY [JSON.INT s1, JSON.INT s2]
                       ])
              | _ => NONE
            end
        | _ =>
            ( printqf 0
                (Util.dull "\n-- Cannot read return directive from %s --")
                [string test]
            ; NONE
            )
      end

    fun benchSuite {suiteName, suiteDir} =
      let
        val (badTests, measurements) =
          ( Suite.prepareLogDirectory suiteName
          ; MapReduce.map (valOf o one) (Suite.sourceFilesInDir suiteDir)
          )
      in
        { badTests = map (fn (x, y, z) => (z, JSON.NULL)) badTests
        , goodTests = map (fn ((x, y, z), v) => (z, v)) measurements
        }
      end
  end

end
