


type gameobject = {
  x: int;
  y: int;
  width: int;
  height: int;
  deadly: bool;
  image: string;
}


let pressed_keys = ref []
let player_pos = ref (100, 300)
let player_width, player_height = 20, 32

let objects = ref [
  {
    x = 500;
    y = 700;
    width = 32;
    height = 16;
    deadly = false;
    image = "img/block.png";
  };
  {
    x = 300;
    y = 600;
    width = 32;
    height = 16;
    deadly = false;
    image = "img/block.png";
  };
]

let width = 800
let height = 600

let move_speed = 6
let fall_speed = ref 6

let key_left = 65
let key_right = 68


let debug_error str = Firebug.console##error (str);;
let debug_print str = Firebug.console##log (str);;
let print_exn exn = debug_error (Js.string (Printexc.to_string exn))

let isign n = 
  if n > 0 then 1
  else if n < 0 then -1
  else 0

let catching_bind t next handle_exn =
  Lwt.bind t (fun () -> Lwt.catch next (fun exn -> handle_exn exn; Lwt.return_unit))

let intersects o1 o2 =
  o1.x < o2.x + o2.width &&
  o1.x + o1.width > o2.x &&
  o1.y < o2.y + o2.height &&
  o1.y + o1.height > o2.y

let any_intersects obj objects =
  List.exists (fun o -> intersects o obj) objects

let get_canvas () =
  let d = Dom_html.window##document in
  let c = Dom_html.getElementById "c" in
  let c = Js.Opt.get (Dom_html.CoerceTo.canvas c) (fun () -> failwith "no canvas") in 
  let ctx = c##getContext (Dom_html._2d_) in
  c##width <- width;
  c##height <- height;
  ctx

let make_image src =
  let img = Dom_html.createImg Dom_html.document in
  img##src <- Js.string src;
  img

let rec trace_move obj dir objects =
  if dir = (0, 0) then
    obj
  else
    let dx = isign (fst dir) in
    let dy = if dx = 0 then
      isign (snd dir)
    else
      0
    in
    let obj' = { obj with
      x = obj.x + dx;
      y = obj.y + dy;
    } in
    if any_intersects obj' objects then
      obj
    else
      let dir' = (fst dir - dx, snd dir - dy) in
      trace_move obj' dir' objects

let redraw ctx player =
  ctx##clearRect (0.0, 0.0, float width, float height);
  let player_x, player_y = !player_pos in
  let view_x = 0 in
  let view_y = player_y - height / 2 in
  let draw_object o =
    let x = float (o.x - view_x) in
    let y = float (o.y - view_y) in
    ctx##drawImage ((make_image o.image), x, y)
  in
  List.iter draw_object !objects;
  draw_object player


let step ctx =
  let player_object = {
    x = fst !player_pos;
    y = snd !player_pos;
    width = player_width;
    height = player_height;
    deadly = false;
    image = "img/player.png"
  } in
  let dx =
  if CCList.Set.mem key_left !pressed_keys then
    -1
  else if CCList.Set.mem key_right !pressed_keys then
    1
  else
    0
  in
  let dx = dx * move_speed in
  let dy = !fall_speed in
  let player_object = trace_move player_object (dx, dy) !objects in
  player_pos := (player_object.x, player_object.y);
  redraw ctx player_object;
  ()

let rec loop ctx =
  catching_bind
    (Lwt_js.sleep 0.030)
    (fun () -> step ctx; loop ctx)
    print_exn

let start _ =
  let ctx = get_canvas () in
  ignore (loop ctx);
  Js._false

let keydown e =
  let key = e##keyCode in
  (* debug_print key; *)
  pressed_keys := key :: !pressed_keys;
  Js._true

let keyup e =
  let key = e##keyCode in
  pressed_keys := CCList.filter (fun k -> k != key) !pressed_keys;
  Js._false

let main () =
  Random.self_init ();
  let add el ev handler =
    ignore (Dom_html.addEventListener el ev (Dom_html.handler handler) Js._false)
  in
  add Dom_html.window Dom_html.Event.load start;
  add Dom_html.window Dom_html.Event.keydown keydown;
  add Dom_html.window Dom_html.Event.keyup keyup


let () = main ()

