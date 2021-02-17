(* You don't have to import the `FOO` signature. *)
(* It's been done in build.cm already. *)

structure Csc330 = 

struct

(* 
  you will need to print a dot. Use this string to do that.
*)
val dot = "."

(* 
   convert a string to an int. Returns an OPTION
*)
val fromString = Int.fromString

(* 
   read the contents stdin
   and return them as a single string
   (contains end of line separators)
  *)

fun read_stdin () :string = 
    TextIO.inputAll TextIO.stdIn
    handle Io =>
           let 
             val _ = print "error standard input\n"
             val _ = OS.Process.exit(OS.Process.failure)
           in
             ""
           end
                  
(* 
   read the contents of file named fileName 
   and return them as a single string
   (contains end of line separators)
*)
fun read_file fileName :string =
    let
      val input = TextIO.openIn fileName
      val l = TextIO.inputAll input
      val _ = TextIO.closeIn input
    in
      l
    end
    handle Io  => 
           let 
             val _ = print ("error trying to read file " ^ fileName ^ "\n")
             val _ = OS.Process.exit(OS.Process.failure)
           in
             ""
           end



(*
    split a string into a list of strings
    at character st
*)
fun split_at (st, ch) =
    String.tokens (fn c => c = ch ) st

(*
   convert int to real
*)
val int_to_real = Real.fromInt

(*
   convert int to string
*)
val int_to_string = Int.toString

(*
   convert real to string
*)
val real_to_string = Real.toString

(*

*)
fun exit() = OS.Process.exit(OS.Process.success)

fun failure() = OS.Process.exit(OS.Process.failure)

exception DoNotUse

(* the following functions cannot be used *)

fun fold x y =
    raise DoNotUse

fun mapPartial  x y = 
    raise DoNotUse

fun foldl x y =
    raise DoNotUse

fun foldr x y =
    raise DoNotUse

fun map x y =
    raise DoNotUse

fun app x y =
    raise DoNotUse

          
fun parse_command_line args = 
    let
      val _ = if length(args) <> 3 then
                let
                  val _ = print "illegal number of command line parameters\n"
                  val _ = failure()
                in
                  0
                end
              else 0
      val binary::fileName::offsetSt:: _ = args
    in
      (binary, fileName, offsetSt) 
    end

        

end              
