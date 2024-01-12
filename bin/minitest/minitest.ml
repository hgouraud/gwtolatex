open Markup

type name = string * string

type my_tree =
  | Text of string
  | Element of string * (name * string) list * my_tree list

let main () =
  let body =
    {|First line<br>
    <span style="display:none" mode="tex">\\index{Aaaa, Bbbb}</span><br>
    Second line<br>|}
  in
  let tree =
    body |> string |> parse_html |> signals
    |> tree
         ~text:(fun ss -> Text (String.concat "" ss))
         ~element:(fun (_, name) attributes children ->
           Element (name, attributes, children))
  in

  match tree with
  | Some tree ->
      let rec loop tree =
        match tree with
        | Text s -> Printf.eprintf "Text element: \"%s\"\n" s
        | Element (name, _attributes, children) ->
            Printf.eprintf "Element: \"%s\"\n" name;
            (match name with
            | "span" -> Printf.eprintf "Children: (%d)\n" (List.length children)
            | "br" -> Printf.eprintf "\n"
            | _ -> Printf.eprintf "Other: %s\n" name);
            List.iter (fun ch -> loop ch) children
      in
      loop tree
  | _ -> Printf.eprintf "Bad tree\n"

let _ = main ()
