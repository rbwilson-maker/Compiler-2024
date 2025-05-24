
(* probably should drain this into structures with more sensible organisation *)
structure Util :> UTIL =
struct
  fun signalEq (signal1, signal2) = signal1 = signal2
  fun signalIsAlarm signal = signalEq (signal, Posix.Signal.alrm)
  fun statusIsAlarm (Posix.Process.W_SIGNALED signal) = signalIsAlarm signal
    | statusIsAlarm _ = false

  fun ansi code s = if !Config.color then code ^ s ^ "\^[[0m" else s
  fun bold s  = ansi "\^[[1m" s
  fun dull s = ansi "\^[[2m" s
  fun red s = ansi "\^[[31m" s
  fun yellow s = ansi "\^[[33m" s
  fun green s = ansi "\^[[32m" s
  fun blue s = ansi "\^[[34m" s

  fun trim line =
    if String.sub (line, size line - 1) = #"\n"
    then String.extract (line, 0, SOME (size line - 1))
    else line

  fun head filename =
    let
      val stream = TextIO.openIn filename
      val line = valOf (TextIO.inputLine stream before TextIO.closeIn stream)
    in
      trim line
    end handle _ => ""

  fun mkdir dir = (
    if not (OS.FileSys.access (dir, []))
    then OS.FileSys.mkDir dir
    else if not (OS.FileSys.isDir dir)
    then failwithf "Exists but is not a directory: `%s`" [string dir]
    else ()
  ) handle exn => failwithf "Error making directory %s: %s" [ string dir, string (exnMessage exn) ]

  fun withRefSetTo r x f =
    let val old = !r
        val () = r := x;
        val result = f ()
    in
      r := old;
      result
    end

  fun isDir s = OS.FileSys.isDir s handle _ => false
  fun isLink s = OS.FileSys.isLink s handle _ => false
  fun isRead s = OS.FileSys.access (s, [ OS.FileSys.A_READ ]) handle _ => false
  fun isExec s = OS.FileSys.access (s, [ OS.FileSys.A_EXEC ]) handle _ => false

  (* Flags necessary for reading and writing. *)
  val rw =
    let
      open Posix.FileSys.S
    in
      flags [ irusr, iwusr, irgrp, iwgrp ]
    end

  fun ls dir =
    let
      val dirstream = OS.FileSys.openDir dir
      val nextFile = fn () => OS.FileSys.readDir dirstream
      fun shouldSkip file =
        isDir file orelse (not (!Config.symlink) andalso isLink file)

      (* Add all files in the directory. *)
      fun loop NONE set = set
        | loop (SOME file) set =
            loop (nextFile ()) (
              if shouldSkip (dir // file)
              then set
              else StringSet.add (set, file)
            )

    in
      StringSet.listItems (loop (nextFile ()) StringSet.empty)
        before OS.FileSys.closeDir dirstream
    end

  fun app f file =
    let
      val stream = TextIO.openIn file
      fun loop () =
        case TextIO.inputLine stream of
          NONE => TextIO.closeIn stream
        | SOME s => (f s; loop ())
    in
      (loop ()) handle exn =>
        ( TextIO.closeIn stream handle _ => ()
        ; raise exn
        )
    end

  fun fold f acc file =
    let
      val stream = TextIO.openIn file
        fun loop acc =
          case TextIO.inputLine stream of
            NONE => (TextIO.closeIn stream ; acc)
          | SOME x => loop (f (trim x, acc))
     in
      (loop acc) handle exn =>
        ( TextIO.closeIn stream handle _ => ()
        ; raise exn
        )
     end

  fun cat file = (
    if isRead file then app print file else ()
  ) handle exn =>
    ( print ("Error encountered printing file "^ file ^ "\n")
    ; print (exnMessage exn ^ "\n")
    )

  (* Like the unix utility: move the `from` file to `to`. *)
  fun mv { from, to } =
    if isRead from
    then OS.FileSys.rename { old = from, new = to }
    else ()
end
