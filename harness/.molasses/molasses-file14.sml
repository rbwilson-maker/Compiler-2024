(* drive/Runner.sml : 1.1-116.1 *)
(* molasses-file14.sml *)
local
	infix 5 **
	infix 5 //
	infixr <|
	infix |>
	open InternalMolassesStructure0
	open InternalMolassesStructure1
in
structure Runner :> RUNNER =
  struct
    val devnull =
      Posix.FileSys.openf
        ("/dev/null", Posix.FileSys.O_WRONLY, Posix.FileSys.O.append)

    (* Redirection:
     *
     * redirectOut (instructions) Posix.FileSys.std{out,err}
     * modifies the behavior of stdout/stderr to silence, redirect to
     * an existing file descriptor, or redirect to a log file (which
     * will be overwritten if it already exists)
     *)
    datatype redirection = UNCHANGED | SILENCE | REDIRECT of Posix.IO.file_desc

    fun close redirect =
      case redirect of
        REDIRECT desc => Posix.IO.close desc
      | _ => ()

    (* PERF: hilariously inefficient redirect case *)
    fun print (redirect, s) =
      case redirect of
        UNCHANGED => TextIO.print s
      | SILENCE => ()
      | REDIRECT desc =>
          (case Posix.Process.fork () of
             SOME pid =>
               (ignore o Posix.Process.waitpid) (Posix.Process.W_CHILD pid, [])
           | NONE =>
               ( Posix.IO.dup2 {old = desc, new = Posix.FileSys.stdout}
               ; TextIO.print s
               ; OS.Process.exit OS.Process.success
               ))

    fun redirectOut UNCHANGED std = ()
      | redirectOut SILENCE std = Posix.IO.dup2 {old = devnull, new = std}
      | redirectOut (REDIRECT sink) std = Posix.IO.dup2 {old = sink, new = std}

    (* This is the main function, the others are helpers *)
    fun redirect {msg, command, args, secs, out, err} =
      let
        val () =
          if not (! Config.debug) then
            ()
          else
            TextIO.output
              ( TextIO.stdErr
              , sprintf "** Forking to run %s %s **\n"
                  [string command, string (String.concatWith " " args)]
              )
        val (pid, status) =
          case Posix.Process.fork () of
            SOME pid => Posix.Process.waitpid (Posix.Process.W_CHILD pid, [])
          | NONE =>
              ( redirectOut out Posix.FileSys.stdout
              ; TextIO.print msg
              ; redirectOut err Posix.FileSys.stderr
              (* Become a process group leader *)
              ; Posix.ProcEnv.setpgid {pid = NONE, pgid = NONE}
              (* Set an alarm, prepare to die *)
              ; ignore (Posix.Process.alarm (Time.fromSeconds secs))
              ; Posix.Process.execp (command, command :: args)
              )
                handle _ => OS.Process.exit OS.Process.failure
      in
        (* Once we've gotten here, it's time for this job to go. Is
         * there a race condition here if another process has claimed
         * this (now terminated) pid in the brief interval? Should we
         * only run kill in the event of a timeout?
         *)
        ( Posix.Process.kill (Posix.Process.K_GROUP pid, Posix.Signal.kill)
            handle _ => ()
        ; status
        )
      end

    fun run {command, args, secs} =
      redirect
        { msg = ""
        , command = command
        , args = args
        , secs = secs
        , out = UNCHANGED
        , err = UNCHANGED
        }

    fun redirectBoth {msg, command, args, secs, fd} =
      redirect
        { msg = msg
        , command = command
        , args = args
        , secs = secs
        , out = REDIRECT fd
        , err = REDIRECT fd
        }

    fun log {command, args, secs, logBase} =
      let
        val outFD = Posix.FileSys.creat (logBase ** SOME "out", Util.rw)
        val errFD = Posix.FileSys.creat (logBase ** SOME "err", Util.rw)
      in
        redirect
          { msg = ""
          , command = command
          , args = args
          , secs = secs
          , out = REDIRECT outFD
          , err = REDIRECT errFD
          } before (Posix.IO.close outFD; Posix.IO.close errFD)
      end
  end

end
