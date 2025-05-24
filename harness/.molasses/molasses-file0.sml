(* utils/TopUtils.sml : 1.1-27.1 *)
(* molasses-file0.sml *)
(* Top-level utility functions *)
infix |>
fun x |> f = f x

infixr <|
fun f <| x = f x

infix 5 //
fun dir // file = OS.Path.joinDirFile {dir = dir, file = file}

infix 5 **
fun base ** ext = OS.Path.joinBaseExt {base = base, ext = ext}

fun id x = x
fun concatMap f xs = List.concat (map f xs)

fun failwith str = raise Fail str
fun failwithf str args = raise Fail (Format.format str args)

structure Option =
  struct
    open Option

    val isNone = fn NONE => true | SOME _ => false
    val isSome = fn NONE => false | SOME _ => true
  end
structure InternalMolassesStructure0 = struct
	val (op |>) = (op |>)
	val (op <|) = (op <|)
	val (op //) = (op //)
	val (op **) = (op **)
	val id = id
	val concatMap = concatMap
	val failwith = failwith
	val failwithf = failwithf
end
