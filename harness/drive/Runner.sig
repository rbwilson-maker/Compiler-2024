signature RUNNER =
sig
  (* Redirection:
   *
   * modifies the behavior of stdout/stderr to silence, redirect to
   * an existing file descriptor, or redirect to a log file (which
   * will be overwritten if it already exists)
   *)
  datatype redirection =
      UNCHANGED
    | SILENCE
    | REDIRECT of Posix.IO.file_desc

  val redirect :
    { msg : string
    , command : string
    , args : string list
    , secs : IntInf.int
    , out : redirection
    , err : redirection
    } -> Posix.Process.exit_status

  val print : redirection * string -> unit
  val close : redirection -> unit

  (* Does not perform any redirection *)
  val run :
    { command : string
    , args : string list
    , secs : IntInf.int
    } -> Posix.Process.exit_status

  (* Redirect standard {out,err} to logbase.{out,err} *)
  val log :
    { command : string
    , args : string list
    , secs : IntInf.int
    , logBase : string
    } -> Posix.Process.exit_status
end
