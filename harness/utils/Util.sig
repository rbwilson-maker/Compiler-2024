signature UTIL =
sig
  val signalEq : Posix.Signal.signal * Posix.Signal.signal -> bool
  val signalIsAlarm : Posix.Signal.signal -> bool
  val statusIsAlarm : Posix.Process.exit_status -> bool


  (* Create output for a terminal that supports ANSI color codes. *)
  val bold : string -> string
  val dull : string -> string
  val red : string -> string
  val yellow : string -> string
  val green : string -> string
  val blue : string -> string

  (* Set ref to value for duration of function call, restoring
   * to old value at end. *)
  val withRefSetTo : 'a ref -> 'a -> (unit -> 'b) -> 'b

  (* Just like the Unix utilities. *)
  val head : string -> string
  val cat : string -> unit
  val mv : { from : string, to : string } -> unit
  val mkdir : string -> unit
  val trim : string -> string

  (* Apply a function to each line of a file. *)
  val app : (string -> unit) -> string -> unit

  (* Fold a function over each line of a file *)
  val fold : (string * 'a -> 'a) -> 'a -> string -> 'a

  (* List all non-directory files in the given directory. *)
  val ls : string -> string list

  (* Flags necessary for reading and writing. *)
  val rw : Posix.FileSys.S.flags

  (* Safe wrappers for checking properties of files. *)
  val isDir : string -> bool
  val isRead : string -> bool
  val isExec : string -> bool
end
