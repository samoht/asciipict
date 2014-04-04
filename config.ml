open Graphics

(*let font = "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"*)

let set_map dimx dimy c =
  clear_graph ();
  moveto 0 0;
  let img = create_image dimx dimy in
  draw_char c;
  blit_image img 0 0;
  dump_image img

let replace_color mat =
  Array.iter
    (fun line ->
       Array.iteri
	 (fun i color -> line.(i) <- if color = 0 then 0 else 255) line) mat;
  mat

type config = { dimx : int;
		dimy : int;
		font : string;
		char_array : (int * int array array) array }

let run ~font =
  open_graph " 30x30";
  set_font font;
  let (dimx,dimy) = text_size "_" in

  let l = ref [] in
  for i = 126 downto 32 do
    let c = char_of_int i in
    l := (i, replace_color (set_map dimx dimy c)) :: !l;
  done;

  let char_array = Array.of_list !l in
  close_graph ();

  { dimx=dimx;
    dimy=dimy;
    font=font;
    char_array=char_array }
