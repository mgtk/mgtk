(* nim.sml                             HR 21/7-00 *)
(*
load "Gtk";
load "ListPair";
load "Date";
load "Time";
*)
val rows = 5
val columns = 8

fun fromMtoN m n = if m > n then [] else m :: (fromMtoN (m+1) n)

val rowNos = fromMtoN 0 (rows-1)
val colNos = fromMtoN 0 (columns-1) 

val state = ref []: int list ref

fun setState m = state :=
      map (fn x => Int.max((x + m) mod (columns+1),1)) [3,5,2,7,6]


datatype message = YouMayWin | YouAreTheWinner | YouAreLoosing | YouLost

fun message widget mes =
    Gtk.label_set_text widget 
       (case mes of
          YouMayWin       => "You may win"
         |YouAreTheWinner => "You are the winner"
         |YouAreLoosing   => "You are loosing"
         |YouLost         => "You lost" )

val _ = Gtk.init(CommandLine.name()::CommandLine.arguments())

fun delete_event _ = false
fun destroy _ = Gtk.main_quit()

fun display butTab state =
    app 
    (fn (bList,count) => 
       (app Gtk.widget_show  (List.take(bList,count)) 
       ; app Gtk.widget_hide (List.drop(bList,count)) ) )
    (ListPair.zip(butTab,!state))

fun attach table (widget,row,col) =
    Gtk.table_attach_defaults table widget  col (col+1) row (row+1)

fun makeButtons (table) =
       map 
       (fn row =>
           map 
           (fn col => 
               let
                  val widg = Gtk.button_new_with_label ("X")
                  val _    = attach table (widg, row, col)
               in
                  widg
               end )
           colNos ) 
       rowNos

fun attachCallBack callBack butTab mes =
    app
    (fn (row,bList) =>
        app 
        (fn (col,button) => 
            Gtk.connect_clicked button (callBack (row,col) butTab mes))
        (ListPair.zip(colNos,bList)))
    (ListPair.zip(rowNos,butTab))

fun xor xs = foldl (fn (x,r) => Word.xorb((Word.fromInt x),r)) 0w0 xs

val isNull = List.all (fn x => x = 0)

fun maxIndex (x::xs) = #1( foldl 
  (fn (x,(i,mx,j)) => if x > mx then (j,x,j+1) else (i,mx,j+1))
  (0,x,1) xs)

fun win(x::xs,xr) = let val y = Word.toInt(Word.xorb(Word.fromInt x,xr))
                    in if y < x then y::xs else x::win(xs,xr) end

fun callBack (row,col) butTab mesW _ = 
    ( Gtk.label_set_text mesW (Int.toString(row)^" "^Int.toString(col)^"\n")
    ; state := List.take(!state,row) @ col :: List.drop(!state,row+1)
    ; if isNull(!state) then
          message mesW YouAreTheWinner
      else 
          (case xor (!state) of
               0w0  => 
               let val i = maxIndex(!state) in
                   state := List.take(!state,i) @ (List.nth(!state,i)-1)
                            :: List.drop(!state,i+1)
                 ; message mesW YouMayWin end
             |xr   => ( state := win(!state,xr)
                      ; message mesW (if isNull(!state) then YouLost
                                      else YouAreLoosing) 
                      )
          )
    ; display butTab state)

fun main () =
    let val window  = Gtk.window_new Gtk.WINDOW_TOPLEVEL
        val box     = Gtk.vbox_new false 0
        val table   = Gtk.table_new columns rows true
        val butTab  = makeButtons(table)
        val mesW    = Gtk.label_new ""
    in
        setState (Date.second(Date.fromTimeLocal(Time.now())))
    ;   attachCallBack callBack butTab mesW
    ;   Gtk.connect_delete_event window delete_event 
    ;   Gtk.connect_destroy window destroy
    ;   Gtk.container_set_border_width window 10
    ;   Gtk.container_add window box
    ;   Gtk.box_pack_start box table true true 0
    ;   Gtk.box_pack_start box mesW true true 0
    ;   display butTab state
    ;   message mesW (if xor(!state) = 0w0 then  YouAreLoosing
                     else YouMayWin)
    ;   Gtk.widget_show mesW
    ;   Gtk.widget_show table
    ;   Gtk.widget_show box
    ;   Gtk.widget_show window
    ;   Gtk.main() 
    end
