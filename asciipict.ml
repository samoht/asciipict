(* ASCIIpict : a program to generate ASCII pictures from PBM or PGM pictures *)

open Printf

let char_rows = ref Chars.dimy
and char_cols = ref Chars.dimx
let char_array = ref Chars.array
let font = ref Chars.font


let string_iter f s =
  for i = 0 to String.length s - 1 do f (String.unsafe_get s i) done

let set_char_array char_subset =
  let ar = Array.make 255 false in
  string_iter (fun c -> ar.(int_of_char c) <- true) char_subset;
  let l = ref [] in
  Array.iter (fun ((i,_) as x) -> if ar.(i) then l:=x::!l) !char_array;
  char_array := Array.of_list !l


let rec input_int in_chan =
  let c = input_char in_chan in
  if c = ' ' or c = '\n' then
    input_int in_chan
  else read (int_of_char c - 48) in_chan

and read i in_chan =
  let c = input_char in_chan in
  if c = ' ' or c = '\n' then i
  else
    read (10*i + (int_of_char c - 48)) in_chan


type buf = { mutable n : int;
	     mutable ar : int array }
let buf = { n = -1; 
	    ar = Array.make 8 0 }
let reset_buf () = buf.n <- -1
let input_bit in_chan =
  let ar = buf.ar in
  if buf.n = -1 then
    begin
      buf.n <- 7;
      let x = ref (input_byte in_chan) in
      for i = 0 to 7 do
	ar.(i) <- !x mod 2;
	x := !x / 2
      done
    end;

  let n = buf.n in
  let i = Array.unsafe_get ar n in
  buf.n <- n - 1;
  i

    

let read_color_pbm in_chan =
  255*(1-(input_int in_chan))

let read_color_pbm_raw in_chan =
  255*(1-(input_bit in_chan))

let read_color_pgm in_chan =
  input_int in_chan

let read_color_pgm_raw in_chan =
  int_of_char (input_char in_chan)


let check_first_line in_chan =
  match input_line in_chan with
      "P1" -> `PBM
    | "P2" -> `PGM
    | "P4" -> `PBM_raw
    | "P5" -> `PGM_raw
    | s-> failwith ("Unrecognized format "^s)

let check_colors_255 in_chan =
  let ncolors = input_int in_chan in
  if ncolors <> 255 then
    failwith (sprintf
		"Only 255 colors PBM format is accepted (%i colors)" ncolors)



let rec skip_comments in_chan =
  let s = input_line in_chan in
  if String.length s > 0 then
    if s.[0] = '#' then skip_comments in_chan (* skips comments *)
    else s
  else skip_comments in_chan (* skips empty lines *)


let parse_dim s =
  let re = Str.regexp "\\([0-9]+\\)[ ]+\\([0-9]+\\)" in
  if Str.string_match re s 0 then
    (int_of_string (Str.matched_group 1 s), 
     int_of_string (Str.matched_group 2 s))
  else failwith "incorrect syntax for the dimensions"

   
let read_file ?(scale=1) ?(color_scale=1.0) in_chan =
  let filetype = check_first_line in_chan in
  let dim_line = skip_comments in_chan in
  let (dimx1,dimy1) = parse_dim dim_line in 

  let read_color =
    match filetype with
	`PBM -> read_color_pbm
      | `PGM -> check_colors_255 in_chan; read_color_pgm
      | `PBM_raw -> read_color_pbm_raw
      | `PGM_raw -> check_colors_255 in_chan; read_color_pgm_raw in
 
  let dimx = scale*dimx1 and dimy = scale*dimy1 in
  if dimx = 0 || dimy = 0 then exit 0;

  let mat = Array.make_matrix dimy dimx 0 in
  for i = 0 to dimy1-1 do
    reset_buf (); (* alignment (raw PBM only) *)
    for j = 0 to dimx1-1 do
      let native_color = read_color in_chan in
      let color = 255 - 
		  (int_of_float 
		     (color_scale *. (float (255 - native_color)))) in
      for shiftx = 0 to scale-1 do
	let row = mat.(scale*i+shiftx) in
	for shifty = 0 to scale-1 do
	  row.(scale*j+shifty) <- color
	done
      done
    done
  done;
  mat

let map_img f img = Array.map (Array.map f) img
let neg img = map_img (fun x -> 255 - x) img

let blit_matrix ~posx ~lenx ~posy ~leny mat =
  let sub = Array.make_matrix lenx leny 0 in
  for i = 0 to lenx-1 do
    let s = sub.(i) and m = mat.(i+posx) in
    for j = 0 to leny -1 do
      s.(j) <- m.(j+posy)
    done
  done;
  sub

let diff mat1 mat2 =
  let dimx = Array.length mat1 
  and dimx' = Array.length mat2 in
  if dimx = 0 or dimx <> dimx' then invalid_arg "diff";
  let dimy = Array.length mat1.(0)
  and dimy' = Array.length mat2.(0) in
  if dimy = 0 or dimy <> dimy' then invalid_arg "diff";

  let sum = ref 0 in
  for i = 0 to dimx-1 do
    let m1 = mat1.(i) and m2 = mat2.(i) in
    for j = 0 to dimy-1 do
      sum := !sum + (abs (m1.(j) - m2.(j)))
    done
  done;
  !sum

let best_diff mat_array mat =
  let init = ((!char_rows * !char_cols * 255),32) in
  let (lower_diff, char_num) =
    Array.fold_left (fun ((d_accu,_) as accu) ((i,mat')) ->
		       let d = diff mat' mat in 
		       if d < d_accu then (d,i)
		       else accu) init mat_array in
  (*eprintf "best_diff=%i\n" lower_diff;*)
  char_of_int char_num


let char_map mat =
  let char_array_loc = !char_array in
  let dimx = Array.length mat
  and dimy = Array.length mat.(0) in
  let dimx_mini = dimx / !char_rows
  and dimy_mini = dimy / !char_cols in
  let splitted_mat = Array.make_matrix dimx_mini dimy_mini ' ' in
  for i = 0 to dimx_mini-1 do
    for j = 0 to dimy_mini-1 do
      let sub_mat = blit_matrix 
		    ~posx:(i * !char_rows) 
		    ~lenx:!char_rows
		    ~posy:(j * !char_cols)
		    ~leny:!char_cols
		      mat in
      splitted_mat.(i).(j) <- best_diff char_array_loc sub_mat
    done
  done;
  splitted_mat

let print_html line =
  let print_c = function
      '<' -> print_string "&lt;"
    | '>' -> print_string "&gt;"
    | '&' -> print_string "&amp;"
(*    | ' ' -> print_string "&nbsp;"*)
    | c -> print_char c in
  Array.iter print_c line;
  print_string "\n"

let print_map ~negative ~html map =
  if html then 
    (let (bg,fg) = 
       if negative then ("black","white") 
       else ("white","black") in
     printf "<html>
<body bgcolor=%s>
<font color=%s>
<pre>
" bg fg;
     Array.iter print_html map;
     print_string "</pre>
</body>
</html>
")
  else 
    Array.iter (fun ar -> Array.iter print_char ar; print_newline ()) map
  

let run ~negative ~verbose ~scale ~color_scale ~html in_chan = 
  let img = read_file ~scale ~color_scale in_chan in
  let img' = if negative then neg img else img in
  let map = char_map img' in
  let height = Array.length map in
  if verbose then
    printf "Font: %s\nWidth: %i\nHeight: %i\n\n"
      !font (if height > 0 then Array.length map.(0) else 0) height;
  print_map ~negative ~html map

open Arg

let errmsg = Printf.sprintf 
		"Usage: asciipict [options] [input file]
This program produces an ASCII representation of an image.
Images must be in PBM or PGM formats (variants of the PNM format).

Default font:\n%s\n\nOptions are:"
	       !font

let negative = ref false
let scale = ref 1
let brighter = ref 0.0
let verbose = ref false
let html = ref false
let char_subset = ref None
let rec options = [
  "-negative",
  Set negative,
  "         reverse video";

  "-scale",
  Int (fun i -> 
	 if i > 0 then scale := i 
	 else (usage options errmsg; exit 1)),
  "<factor>    factor must be a strictly positive integer";

  "-brighter",
  Float (fun f -> 
	 if f >= 0.0 && f <= 1.0 then brighter := f
	 else (usage options errmsg; exit 1)),
  "<num>    num must be a positive number lower or equal to 1.0";

  "-font",
  String (fun s -> font := s; 
	    let config = Config.run ~font:s in
	    char_cols := config.Config.dimx;
	    char_rows := config.Config.dimy;
	    char_array := config.Config.char_array),
  "<font>       changes the default font";

  "-html",
  Set html,
  "             HTML output";

  "-subset",
  String (fun s -> char_subset := Some s),
  "<string>   subset of characters to use";

  "-verbose",
  Unit (fun () -> verbose := true),
  "          prints additional information";
]


let _ =
  let files = ref [] in
  parse options (fun file -> files := file::!files)
      errmsg;

  (match !char_subset with
       None -> ()
     | Some s -> set_char_array s);

  let color_scale = 1.0 -. !brighter in

  if !files = [] then 
    run ~negative:!negative ~verbose:!verbose ~scale:!scale ~color_scale 
    ~html:!html stdin
  else 
    List.iter (fun file -> run 
		 ~negative:!negative
		 ~verbose:!verbose 
		 ~scale:!scale 
		 ~color_scale 
		 ~html:!html
		 (open_in file)) !files
