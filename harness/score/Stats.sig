signature STATS =
sig
  type 'a result =
    { suite : string
    , badTests : (string * string) list
    , goodTests :
      ((string * 'a * string) * (Summary.cat * Summary.res)) list
    } list

  val report : 'a result -> unit
end
