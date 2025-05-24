(* utils/MapReduce.sml : 1.1-74.1 *)
(* molasses-file8.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
structure MapReduce =
  struct

    type value = JSON.value

    datatype 'a workitem =
      VALUE of 'a * value
    | FAILURE of 'a
    | AWAIT of 'a * Posix.Process.pid * string

    (* Spawn a new process to do some work, return pid and pipe *)
    fun start (mapper : 'a -> value) (x : 'a) =
      let
        val temp = OS.FileSys.tmpName ()
        (* XXX replace with a pipe? *)
        fun fork temp mapper x =
          let
            val value = mapper x
            val outstream = TextIO.openOut temp
            val string = JSONPrinter.print (outstream, value)
            val _ = TextIO.closeOut outstream
          in
            OS.Process.exit OS.Process.success
          end
            handle exn => OS.Process.exit OS.Process.failure
      in
        case Posix.Process.fork () of
          NONE => fork temp mapper x
        | SOME pid => AWAIT (x, pid, temp)
      end

    (* Given a terminated process, find in worklist and replace with value *)
    fun join (res as (pid, status)) =
      fn [] => []
      (* Pid not found! (Unexpected, possible error) *)
        | AWAIT (x, pid', tmp) :: work =>
         (if pid <> pid' then
            AWAIT (x, pid', tmp) :: join (pid, status) work
          else
            case status of
              Posix.Process.W_EXITED =>
                VALUE (x, JSONParser.parseFile tmp)
                :: work
                before OS.FileSys.remove tmp
            | _ =>
                FAILURE x
                :: work
                before (OS.FileSys.remove tmp handle _ => ()))
       | item :: work => item :: join res work

    fun map (mapper : 'a -> value) (xs : 'a list) : ('a list * ('a * value) list) =
      let
        fun loop fail succeed [] [] = (rev fail, rev succeed)
          | loop fail succeed (FAILURE x :: worklist) xs =
            loop (x :: fail) succeed worklist xs
          | loop fail succeed (VALUE (x, v) :: worklist) xs =
            loop fail ((x, v) :: succeed) worklist xs
          | loop fail succeed worklist xs =
            let
              val res = Posix.Process.wait ()
              val (worklist, xs) =
                case xs of
                  [] => (join res worklist, [])
                | x :: xs => (join res worklist @ [start mapper x], xs)
            in
              loop fail succeed worklist xs
            end

        val n = ! Config.PARALLELISM
        val (worklist : 'a workitem list, xs : 'a list) =
          if length xs < n then
            (List.map (start mapper) xs, [])
          else
            (List.map (start mapper) (List.take (xs, n)), List.drop (xs, n))
      in
        loop [] [] worklist xs
      end
  end

end
