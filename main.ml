let pi = acos (-1.) (*No Pi constant?..*)
let ($) f g = fun x -> f (g x) (*from devkit*)

let str2d i =
  if i = 0 then "36" else
  if i < 10 then "0" ^ (string_of_int i)
  else string_of_int i

module Model = struct
(*
  wind course;
  wind speed;
  runway heading;
  true speed;
*)
type t = { wc: int; ws: int; rh: int; ts: int; }
let init = { wc = 90; ws = 10; rh = 10; ts = 107; }
let get t id =
  match id with
  | "wc" -> t.wc
  | "ws" -> t.ws
  | "rh" -> t.rh
  | "ts" -> t.ts
  | _ -> assert false

let headwind t = (float t.ws) *. cos (((float t.rh) *. 10. -. (float t.wc)) *. pi /. 180.)
let crosswind t = (float t.ws) *. sin (((float t.rh) *. 10. -. (float t.wc)) *. pi /. 180.)

let wca t angle =
  let awa = ((float t.wc) -. angle) *. pi /. 180. in
  (asin ((float t.ws) *. (sin awa) /. (float t.ts))) /. pi *. 180.

let gs t angle =
  let wca = (wca t angle) *. pi /. 180. in
  let b = float t.ts in
  let a = angle *. pi /. 180. in
  let c = (float t.wc) *. pi /. 180. in
  let d = float t.ws in
  sqrt (b *. b +. d *. d -. 2. *. b *. d *. (cos (c -. a -. wca)))
end

module Event = struct
type t =
  | UpdWC of int
  | UpdWS of int
  | UpdRH of int
  | UpdTS of int

(* return new model *)
let handle t m =
  match t with
  | UpdWC i -> { m with Model.wc = i; }
  | UpdWS i -> { m with ws = i; }
  | UpdRH i -> { m with rh = i; }
  | UpdTS i -> { m with ts = i; }
end

module View = struct

open D3

(** get target from event e, coerce to tp, and retrieve value *)
let get_val e tp =
  match Js.Opt.to_option e##.target with
  | None -> assert false
  | Some t ->
    let i = Js.coerce t tp (fun _ -> assert false) in
    int_of_string @@ Js.to_string i##.value

let update k new_val id =
  match id with
  | "wc" -> k (Event.UpdWC new_val)
  | "ws" -> k (Event.UpdWS new_val)
  | "rh" -> k (Event.UpdRH new_val)
  | "ts" -> k (Event.UpdTS new_val)
  | _ -> assert false

let range_with_label min max k id txt =
  static "div"
    |. seq [ static "input"
      |. str attr "type" "range"
      |. int attr "min" min
      |. int attr "max" max
      |. str attr "id" id
      |. str attr "name" id (*TODO really need?*)
      |. property "value" (fun _ m _ -> Js.string @@ string_of_int @@ Model.get m id)
      |. E.input (fun e _m _ ->
        let new_val = get_val e Dom_html.CoerceTo.input in
        update k new_val id
        )
      ; static "label"
        |. str attr "for" id
        |. text (fun _ _ _ -> txt ^ ": ")
      ; static "input"
        |. str attr "type" "number"
        |. int attr "min" min
        |. int attr "max" max
        |. str attr "id" ("edit_" ^ id)
        |. E.input (fun e _m _ ->
          let new_val = get_val e Dom_html.CoerceTo.input in
          update k new_val id
          )
        |. property "value" (fun _ m _ -> Js.string @@ string_of_int @@ Model.get m id)
    ]

let text_field k id txt =
  static "div"
    |. seq [ static "label"
      |. text (fun _ _ _ -> txt ^ ": ")
      ;
      static "input"
      |. str attr "type" "number"
      |. int attr "min" 1
      |. int attr "max" 530
      |. str attr "id" ("edit_" ^ id)
      |. E.input (fun e _m _ ->
        let new_val = get_val e Dom_html.CoerceTo.input in
        update k new_val id
        )
      |. property "value" (fun _ m _ -> Js.string @@ string_of_int @@ Model.get m id)
    ]

let runway_image radius =
  let svg =
    static "svg"
    |. int attr "height" radius
    |. int attr "width" radius
  in

  let center = radius / 2 in

  let line ?(dashed=false) offset =
    let ret = static "line"
    |. int attr "x1" (offset + center)
    |. int attr "y1" 0
    |. int attr "x2" (offset + center)
    |. int attr "y2" radius
    |. str attr "stroke" "gray"
    |. int attr "stroke-width" 5
    in
    if dashed then ret |. str attr "stroke-dasharray" "5,5" else ret
  in
  let marking1 =
    static "text"
    |. int attr "x" (center - 18)
    |. int attr "y" radius
    |. str attr "style" "font:bold 30px sans-serif"
    |. text (fun _ m _ -> str2d m.Model.rh)
  in
  let marking2 =
    static "text"
    |. str attr "transform" ("rotate(180 " ^ (string_of_int center) ^ " 0)")
    |. int attr "x" (center - 18)
    |. int attr "y" 0
    |. str attr "style" "font:bold 30px sans-serif"
    |. text (fun _ m _ -> str2d ((18 + m.Model.rh) mod 36))
  in
  let runway =
    static "g"
    |. attr "transform" (fun _ m _ -> "rotate(" ^ (string_of_int (m.Model.rh * 10)) ^ " " ^ (string_of_int center) ^ " " ^ (string_of_int center) ^ ")")
    |. seq [ line (-20); line 20; line 0 ~dashed:true; marking1; marking2 ]
  in
  let wind =
    static "polygon"
    |. str attr "points" "130,0 150,30 170,0"
    |. str attr "style" "fill:red;stroke:blue;stroke-width:1"
    |. attr "transform" (fun _ m _ -> "rotate(" ^(string_of_int m.Model.wc) ^ " 150 150)")
  in
  svg <.> seq [ runway; wind ]

let wind_circle =
  let svg =
    static "svg"
    |. int attr "height" 300
    |. int attr "width" 300
  in

  let circle =
    static "circle"
    |. int attr "cx" 150
    |. int attr "cy" 150
    |. int attr "r" 130
    |. str attr "stroke" "black"
    |. int attr "stroke-width" 3
    |. str attr "fill" "white"
  in
  let axis () =
    static "line"
    |. int attr "x1" 150
    |. int attr "y1" 10
    |. int attr "x2" 150
    |. int attr "y2" 290
    |. str attr "stroke" "black"
  in
  let mytext angle radius =
    let x = (int_of_float @@ ((sin (angle *. pi /. 180.)) *. (float radius))) + 150 in
    let y = - (int_of_float @@ ((cos (angle *. pi /. 180.)) *. (float radius))) + 150 in
    static "text"
    |. int attr "x" x
    |. int attr "y" y
    |. str attr "style" "font: 10px sans-serif"
  in
  let angles = [360.; 45.; 90.; 135.; 180.; 225.; 270.; 315.] in
  let wind_correction = static "g" |. seq @@ List.map (fun angle ->
    mytext angle 110
    |. text (fun _ m _ ->
      let angle = Model.wca m angle in
      Printf.sprintf "%.2f" angle)
  ) angles in
  let ground_speed = static "g" |. seq @@ List.map (fun angle ->
    mytext angle 140
    |. text (fun _ m _ ->
      let angle = Model.gs m angle in
      Printf.sprintf "%.2fkn" angle)
  ) angles in
  svg <.> seq [ circle; axis (); axis () |. str attr "transform" "rotate(90 150 150)"; wind_correction; ground_speed ]

let items_of_model k =
  static "div"
    |. seq [
      range_with_label 0 360 k "wc" "Wind from";
      range_with_label 0 50 k "ws" "Wind velocity";
      range_with_label 0 36 k "rh" "Runway heading";
      text_field k "ts" "True airspeed";
      static "label" |. text (fun _ m _ -> "Crosswind component (knots): " ^ (Printf.sprintf "%.2f" @@ Model.crosswind m));
      static "br";
      static "label" |. text (fun _ m _ -> "Headwind  component (knots): " ^ (Printf.sprintf "%.2f" @@ Model.headwind m));
      static "br";
      runway_image 300;
      static "br";
      wind_circle;
    ]

let make k = items_of_model k

end

let main_lazy () =
  let model = ref Model.init in
  let rec go () =
    D3.run ~node:(Dom_html.document##.body) (Lazy.force view) !model and
    view = lazy (View.make (fun e -> model := Event.handle e !model; go ()))
  in
  go ()

let () = main_lazy ()
