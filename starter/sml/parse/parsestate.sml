(* L1 Compiler
 * Parse State System
 * Author: Kaustuv Chaudhuri <kaustuv+@cs.cmu.edu>
 * Annotations: Alex Vaynberg <alv@andrew.cmu.edu>
 * Modified: Frank Pfenning <fp@cs.cmu.edu>
 * Modified: Thea Brick <tbrick@andrew.cmu.edu>
 *
 * This tracks filename and newline characters
 * so character positions in lexer tokens
 * can be converted to line.column format for error messages
 *)
structure ParseState :> PARSE_STATE =
struct

  val currFilename = ref ""
  val currLines = ref (nil : int list)

  fun setfile filename =
      (currFilename := filename;
       currLines := nil)

  fun newline pos =
      (currLines := pos :: !currLines)

  (* look (pos, newline_positions, line_number) = (line, col)
   * pos is buffer position
   * newline_positions is (reverse) list of newline positions in file
   * line_number is lenght of newline_positions
   *)
  fun look (pos, newline_positions, n) =
    case newline_positions of
      [] => (1, pos-1) (* first line pos is off by 1 *)
    | a :: rest =>
        (* a is end of line n *)
        if a < pos then (n+1, pos-a)
        else look (pos, rest, n-1)

  (* ext (leftpos, rightpos) = SOME((leftline, leftcol), (rightline, rightcol), filename)
   * return NONE for invalid position (0,0)
   *)
  fun ext (0, 0) = NONE
    | ext (left, right) =
        SOME ( look (left, !currLines, List.length (!currLines))
             , look (right, !currLines, List.length (!currLines))
             , !currFilename
             )

end
