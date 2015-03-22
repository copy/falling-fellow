


type gameobject = {
  mutable x: int;
  mutable y: int;
  width: int;
  height: int;
  deadly: bool;
  blocking: bool;
  mutable image: string;
}

type direction =
  | Left
  | Right

let dx_of_direction = function
  | Left -> -1
  | Right -> 1

let name_of_direction = function
  | Left -> "left"
  | Right -> "right"

let pressed_keys = ref []
let player_pos = ref (100, 300)
let player_direction = ref Left
let player_width, player_height = 20, 32

let objects = ref [
  {
    x = 500;
    y = 700;
    width = 32;
    height = 16;
    deadly = false;
    blocking = true;
    image = "img/block.png";
  };
  {
    x = 300;
    y = 600;
    width = 32;
    height = 16;
    deadly = false;
    blocking = true;
    image = "img/block.png";
  };
  {
    x = 300;
    y = 700;
    width = 8;
    height = 16;
    deadly = true;
    blocking = true;
    image = "img/spike.png";
  };
]

let width = 800
let height = 600

let initial_fall_speed = 3.0
let fall_accel = 0.02
let move_speed = 4
let fall_speed = ref initial_fall_speed

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

let intersecting_object obj objects =
  try
    Some (List.find (fun o -> intersects o obj) objects)
  with
    Not_found -> None

let get_canvas () =
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
    (obj, None)
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
    match intersecting_object obj' objects with
      | None ->
        let dir' = (fst dir - dx, snd dir - dy) in
        trace_move obj' dir' objects
      | Some other_object ->
        (obj, Some other_object)

let redraw ctx player view_y =
  ctx##clearRect (0.0, 0.0, float width, float height);
  let draw_object o =
    let x = float o.x in
    let y = float (o.y - view_y) in
    ctx##drawImage ((make_image o.image), x, y)
  in
  List.iter draw_object !objects;
  draw_object player

let reposition start_y o =
  o.x <- Random.int width;
  o.y <- Random.int height + start_y + height

let reposition_objects view_y =
  let out_of_view o = o.y + o.height < view_y in
  let repositionable_objects = List.filter out_of_view !objects in
  List.iter (reposition view_y) repositionable_objects;
  ()

let step ctx =
  let player_x, player_y = !player_pos in
  let view_y = player_y - height / 2 in
  let player_object = {
    x = player_x;
    y = player_y;
    width = player_width;
    height = player_height;
    deadly = false;
    blocking = false;
    image = ""
  } in
  let dx =
  if CCList.Set.mem key_left !pressed_keys then
    (player_direction := Left; -1)
  else if CCList.Set.mem key_right !pressed_keys then
    (player_direction := Right; 1)
  else
    0
  in
  let dx = dx * move_speed in
  let dy = int_of_float !fall_speed in
  let (player_object, other_object) = trace_move player_object (dx, dy) !objects in
  player_pos := (player_object.x, player_object.y);
  let dead, blocked =
  match other_object with
    | Some obj when obj.deadly -> true, false
    | Some obj -> false, true
    | None -> false, false
  in
  if dead then begin
    Dom_html.window##alert (Js.string "You died! Try again");
    false
  end
  else
    let state = if blocked then "standing" else "falling" in
    player_object.image <- Printf.sprintf "img/player_%s_%s.png" state (name_of_direction !player_direction);
    begin
    if blocked then
      fall_speed := initial_fall_speed
    else
      fall_speed := !fall_speed +. fall_accel
    end;
    redraw ctx player_object view_y;
    reposition_objects view_y;
    true


let rec loop ctx =
  catching_bind
    (Lwt_js.sleep 0.016)
    (fun () -> if step ctx then loop ctx else Lwt.return_unit)
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

