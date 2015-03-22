


type gameobject = {
  pos: int * int;
  size: int * int;
  deadly: bool;
  image: string;
}


let pressed_keys = ref []
let player_pos = ref (100, 300)

let objects =  ref [
  {
    pos = (500, 700);
    size = (32, 32);
    deadly = false;
    image = "img/block.png";
  }
]

let width = 800
let height = 600



let debug_error str = Firebug.console##error (str);;
let debug_print str = Firebug.console##log (str);;
let print_exn exn = debug_error (Js.string (Printexc.to_string exn))

let catching_bind t next handle_exn =
  Lwt.bind t (fun () -> Lwt.catch next (fun exn -> handle_exn exn; Lwt.return_unit))


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

let redraw ctx =
  ctx##clearRect (0.0, 0.0, float width, float height);
  let player_x, player_y = !player_pos in
  let view_x = 0 in
  let view_y = player_y - height / 2 in
  let draw_object o =
    let x = float (fst o.pos - view_x) in
    let y = float (snd o.pos - view_y) in
    ctx##drawImage ((make_image o.image), x, y)
  in
  List.iter draw_object !objects;
  draw_object {
    pos = !player_pos;
    size = 32, 32;
    deadly = false;
    image = "img/player.png"
  }


let step ctx =
  redraw ctx;
  let player_x, player_y = !player_pos in
  player_pos := (player_x, player_y + 5);
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
  debug_print key;
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

