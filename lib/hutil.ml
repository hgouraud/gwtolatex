(* html utilities *)
(* v1  Henri, 2023/10/16 *)

let test_attr attributes attr value =
  List.exists (fun ((_, k), v) -> k = attr && v = value) attributes

let get_attr attributes attr =
  List.fold_left
    (fun c ((_, k), v) -> if k = attr then v ^ c else c)
    "" attributes

(* <a href="base?m=IM&p=first_name&n=surname&occ=noc&k=first_name.noc.surname" *)
(* <a href="base?m=IM;s=test/filaname.jpg"> *)
(* <a href="base_token?m=IM;s=test/filaname.jpg"> *)
(* Chausey\_{}qnnvsntxq?templ=tex\&{}m=IM *)

let split_href href =
  let href = Sutil.convert_html href |> Sutil.decode in
  let parts = String.split_on_char '?' href in
  let href =
    Sutil.replace ';' '&'
      (List.nth parts (if List.length parts = 2 then 1 else 0))
  in
  let evars = String.split_on_char '&' href in
  let evars =
    List.map
      (fun kv ->
        let tmp = String.split_on_char '=' kv in
        (List.nth tmp 0, if List.length tmp > 1 then List.nth tmp 1 else ""))
      evars
  in
  let evars =
    (* & have been escaped as \&{} !! *)
    List.map
      (fun (k, v) ->
        ( (if String.length k > 2 && k.[0] = '{' && k.[1] = '}' then
             String.sub k 2 (String.length k - 2)
           else k),
          if String.length v > 0 && v.[String.length v - 1] = '\\' then
            String.sub v 0 (String.length v - 1)
          else v ))
      evars
  in
  if List.mem_assoc "b" evars then evars
  else
    let server = List.nth parts 0 in
    let b =
      let j = try String.index server '_' with Not_found -> -1 in
      if j <> -1 then String.sub server 0 j else server
    in
    ("b", b) :: evars
(* TODO treat CGI case *)

let get_href_attr attr attrl =
  if List.mem_assoc attr attrl then List.assoc attr attrl else ""
