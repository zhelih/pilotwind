let pi = acos (-1.) (*No Pi constant?..*)
let ($) f g = fun x -> f (g x) (*from devkit*)

module Model = struct
type t = { wc: int; ws: int; rh: int; }
let init = { wc = 90; ws = 10; rh = 10; }
let get t id =
  match id with
  | "wc" -> t.wc
  | "ws" -> t.ws
  | "rh" -> t.rh
  | _ -> assert false
end

module Event = struct
type t =
  | UpdWC of int
  | UpdWS of int
  | UpdRH of int

(* return new model *)
let handle t m =
  match t with
  | UpdWC i -> { m with Model.wc = i; }
  | UpdWS i -> { m with ws = i; }
  | UpdRH i -> { m with rh = i; }
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

let runway_image radius =
  let svg =
    static "svg"
    |. int attr "height" radius
    |. int attr "width" radius
  in

  let line offset =
    let xy f angle = int_of_float @@ ((float radius) *. (f ((float angle) *. 10. *. pi /. 180.)) /. 2. +. (float radius) /. 2.) in
    static "line"
    |. attr "x1" (fun _ m _ -> string_of_int @@ xy ((~-.) $ sin) (m.Model.rh + offset))
    |. attr "y1" (fun _ m _ -> string_of_int @@ xy cos (m.Model.rh + offset))
    |. attr "x2" (fun _ m _ -> string_of_int @@ xy sin (m.Model.rh - offset))
    |. attr "y2" (fun _ m _ -> string_of_int @@ xy ((~-.) $ cos) (m.Model.rh - offset))
    |. str attr "stroke" "gray"
    |. int attr "stroke-width" 5
    |. str attr "stroke-dasharray" "5,5"
  in
  let runway =
    static "g"
    |. str attr "transform" "rotate(45)"
    |. seq [ line (-1); line 1 ]
  in
  svg <.> runway

let items_of_model k =
  static "div"
    |. seq [
      range_with_label 0 360 k "wc" "Wind direction";
      range_with_label 0 50 k "ws" "Wind velocity";
      range_with_label 0 36 k "rh" "Runway heading";
      runway_image 300;
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
