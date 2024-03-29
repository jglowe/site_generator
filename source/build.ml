(*
 *
 *
 *
 *)

open Jingoo

exception Invalid_Page of string

let rec yaml_to_jingoo (value : Yaml.value) =
  let yaml_tuple_to_jingoo_tuple (str, value) = (str, yaml_to_jingoo value) in
  match value with
  | `Null -> Jg_types.Tnull
  | `Bool b -> Tbool b
  | `Float f -> Tfloat f
  | `String s -> Tstr s
  | `A l -> Tlist (List.map yaml_to_jingoo l)
  | `O l -> Tobj (List.map yaml_tuple_to_jingoo_tuple l)

let yaml_tuple_to_jingoo_tuple (str, value) = (str, yaml_to_jingoo value)

let yaml_to_html_endings filename =
  let regex = Str.regexp "\\(.+\\)\\.y\\(a\\)?ml$" in
  Str.global_replace regex "\\1.html" filename

let yaml_to_html_endings_fpath filename =
  let filename = Fpath.to_string filename in
  let filename = yaml_to_html_endings filename in
  Fpath.v filename

let remove_html_endings filename =
  let regex = Str.regexp "\\(.+\\)\\.htm\\(l\\)?$" in
  Str.global_replace regex "\\1" filename

let remove_html_endings_fpath filename =
  let filename = Fpath.to_string filename in
  let filename = remove_html_endings filename in
  Fpath.v filename

let site_wide_variables =
  let content =
    match Yaml_unix.of_file (Fpath.v "site.yml") with
    | Ok v -> v
    | _ -> Yaml_unix.of_file_exn (Fpath.v "site.yaml")
  in
  match content with
  | `O assoc_list -> assoc_list
  | _ -> raise (Invalid_Page "Root yaml value is not an object")

let get_page_content content_file =
  let content = Yaml_unix.of_file_exn content_file in
  match content with
  | `O assoc_list -> assoc_list
  | _ -> raise (Invalid_Page "Root yaml value is not an object")

let parse_datetime datetime = Core.Time.of_string datetime

let format_datetime datetime =
  let timezone = Core.Time.Zone.of_utc_offset ~hours:0 in
  Core.Time.format datetime "%a %b %d, %Y" ~zone:timezone

let filters =
  [ ( "markdown",
      Jg_types.Tfun
        (fun ?(kwargs = []) value ->
          match kwargs with
          | _ -> (
            match value with
            | Jg_types.Tstr s -> Jg_types.Tstr (Omd.to_html (Omd.of_string s))
            | a -> a )) );
    ( "date",
      Jg_types.Tfun
        (fun ?(kwargs = []) value ->
          match kwargs with
          | _ -> (
            match value with
            | Jg_types.Tstr s ->
                Jg_types.Tstr (format_datetime (parse_datetime s))
            | a -> a )) ) ]

let render_page ~templates_dir ~template (output_file, page_content) =
  print_endline (Fpath.to_string output_file) ;
  let content = page_content @ site_wide_variables in
  (* Convert page content to jingoo *)
  let models = List.map yaml_tuple_to_jingoo_tuple content in
  let template_dir = Fpath.to_string templates_dir in
  let env = {Jg_types.std_env with template_dirs = [template_dir]; filters} in
  let rendered_template =
    Jg_template.from_file (Fpath.to_string template) ~env ~models
  in
  match Bos.OS.File.write output_file rendered_template with
  | Ok _ -> ()
  | Error _ ->
      Printf.printf "Unable to render template to: %s\n"
        (Fpath.to_string output_file)

let get_dir_files dir =
  let is_dir_with_prefix file =
    Sys.is_directory (Fpath.to_string (Fpath.append dir file))
  in
  let not_directory file = not (is_dir_with_prefix file) in
  let content = Array.to_list (Sys.readdir (Fpath.to_string dir)) in
  let content = List.map Fpath.v content in
  List.filter not_directory content

let rec last_list_element_list l =
  match l with [] -> [] | [hd] -> [hd] | _ :: tl -> last_list_element_list tl

let rec remove_last_element_list l =
  match l with
  | [] -> []
  | [_] -> []
  | hd :: tl -> hd :: remove_last_element_list tl

let build_comic ~output_dir ~templates_dir ~contents_dir =
  print_endline "Render the comic pages:-------------------------------------" ;
  let contents_dir = Fpath.append contents_dir (Fpath.v "comic") in
  let page_template =
    Fpath.append templates_dir (Fpath.v "comic/comic.html")
  in
  let content_pages = get_dir_files contents_dir in
  let content_pages = List.map (Fpath.append contents_dir) content_pages in
  let page_content = List.map get_page_content content_pages in
  let page_content =
    List.map (fun a -> a @ site_wide_variables) page_content
  in
  let compare (p1 : (string * Yaml.value) list)
      (p2 : (string * Yaml.value) list) =
    let p1 =
      match List.assoc "comic" p1 with
      | `O assoc_list -> (
        match List.assoc "date" assoc_list with
        | `String s -> Core.Time.of_string s
        | _ -> raise Not_found )
      | _ -> raise Not_found
    in
    let p2 =
      match List.assoc "comic" p2 with
      | `O assoc_list -> (
        match List.assoc "date" assoc_list with
        | `String s -> Core.Time.of_string s
        | _ -> raise Not_found )
      | _ -> raise Not_found
    in
    Core.Time.compare p1 p2
  in
  let page_content = List.sort compare page_content in
  let last = List.length page_content - 1 in
  let render_page = render_page ~templates_dir ~template:page_template in
  (* Renders all the pages *)
  List.iteri
    (fun i page_content ->
      let page_content =
        match List.assoc "comic" page_content with
        | `O assoc_list ->
            ( "comic",
              `O
                ( ("first", `String "/0")
                :: ( "previous",
                     if i = 0 then `String "/0"
                     else `String ("/" ^ string_of_int (i - 1)) )
                :: ( "next",
                     if i = last then `String ("/" ^ string_of_int last)
                     else `String ("/" ^ string_of_int (i + 1)) )
                :: ("latest", `String "/")
                :: assoc_list ) )
            :: List.remove_assoc "comic" page_content
        | _ -> raise Not_found
      in
      let output_file =
        Fpath.append output_dir (Fpath.v (string_of_int i ^ ".html"))
      in
      render_page (output_file, page_content))
    page_content ;
  (* Copies the last post to index.html *)
  if last > 0 then (
    let file =
      Core.In_channel.read_all
        (Fpath.to_string
           (Fpath.append output_dir (Fpath.v (string_of_int last ^ ".html"))))
    in
    Core.Out_channel.write_all ~data:file
      (Fpath.to_string (Fpath.append output_dir (Fpath.v "index.html"))) ;
    print_endline
      (Fpath.to_string (Fpath.append output_dir (Fpath.v "index.html"))) )
  else ()

let build_blog ~output_dir ~templates_dir ~contents_dir =
  print_endline "Render the blog posts:--------------------------------------" ;
  let blog_output_dir = Fpath.append output_dir (Fpath.v "blog") in
  let contents_dir = Fpath.append contents_dir (Fpath.v "blog") in
  let _ = Bos.OS.Dir.create blog_output_dir in
  let page_template = Fpath.append templates_dir (Fpath.v "blog/blog.html") in
  let content_pages = get_dir_files contents_dir in
  let output_files = List.map yaml_to_html_endings_fpath content_pages in
  let output_files = List.map (Fpath.append (Fpath.v "blog")) output_files in
  let content_pages = List.map (Fpath.append contents_dir) content_pages in
  let page_content = List.map get_page_content content_pages in
  let page_content =
    List.map (fun a -> a @ site_wide_variables) page_content
  in
  let combined = List.combine output_files page_content in
  let compare (_, (p1 : (string * Yaml.value) list))
      (_, (p2 : (string * Yaml.value) list)) =
    let p1 =
      match List.assoc "post" p1 with
      | `O assoc_list -> (
        match List.assoc "date" assoc_list with
        | `String s -> Core.Time.of_string s
        | _ -> raise Not_found )
      | _ -> raise Not_found
    in
    let p2 =
      match List.assoc "post" p2 with
      | `O assoc_list -> (
        match List.assoc "date" assoc_list with
        | `String s -> Core.Time.of_string s
        | _ -> raise Not_found )
      | _ -> raise Not_found
    in
    Core.Time.compare p1 p2
  in
  let combined = List.sort compare combined in
  let previous =
    match output_files with
    | [] -> []
    | hd :: _ -> remove_last_element_list (hd :: output_files)
  in
  let next =
    match output_files with
    | [] -> []
    | [hd] -> [hd]
    | _ :: tl -> tl @ last_list_element_list tl
  in
  let combined2 = List.combine previous next in
  let render_page = render_page ~templates_dir ~template:page_template in
  (* Renders all the pages *)
  List.iter2
    (fun (previous, next) (output_file, page_content) ->
      let first = List.hd output_files in
      let page_content =
        match List.assoc "post" page_content with
        | `O assoc_list ->
            ( "post",
              `O
                ( ( "first",
                    `String
                      (Fpath.to_string
                         (Fpath.append (Fpath.v "/")
                            (remove_html_endings_fpath first))) )
                :: ( "previous",
                     `String
                       (Fpath.to_string
                          (Fpath.append (Fpath.v "/")
                             (remove_html_endings_fpath previous))) )
                :: ( "next",
                     `String
                       (Fpath.to_string
                          (Fpath.append (Fpath.v "/")
                             (remove_html_endings_fpath next))) )
                :: ("latest", `String "/blog/")
                :: assoc_list ) )
            :: List.remove_assoc "post" page_content
        | _ -> raise Not_found
      in
      let output_file = Fpath.append output_dir output_file in
      render_page (output_file, page_content))
    combined2 combined ;
  (* Copies the last post to blog/index.html *)
  match last_list_element_list combined with
  | [(output_file, _)] ->
      let file =
        Core.In_channel.read_all
          (Fpath.to_string (Fpath.append output_dir output_file))
      in
      Core.Out_channel.write_all ~data:file
        (Fpath.to_string (Fpath.append output_dir (Fpath.v "blog/index.html"))) ;
      print_endline
        (Fpath.to_string (Fpath.append output_dir (Fpath.v "blog/index.html")))
  | _ -> ()

let build_pages ~output_dir ~templates_dir ~contents_dir =
  print_endline "Render the pages:-------------------------------------------" ;
  let page_template = Fpath.append templates_dir (Fpath.v "page.html") in
  let content_pages = get_dir_files contents_dir in
  let output_files = List.map yaml_to_html_endings_fpath content_pages in
  let content_pages = List.map (Fpath.append contents_dir) content_pages in
  let output_files = List.map (Fpath.append output_dir) output_files in
  let page_content = List.map get_page_content content_pages in
  let page_content =
    List.map (fun a -> a @ site_wide_variables) page_content
  in
  let combined = List.combine output_files page_content in
  List.iter (render_page ~templates_dir ~template:page_template) combined ;
  (* Render 404 page *)
  let page_template = Fpath.append templates_dir (Fpath.v "404.html") in
  render_page ~templates_dir ~template:page_template
    (Fpath.append output_dir (Fpath.v "404.html"), site_wide_variables)

let copy_media ~output_dir ~contents_dir =
  let contents_dir = Fpath.to_string contents_dir in
  let output_dir = Fpath.to_string output_dir in
  match Sys.command ("cp -r " ^ contents_dir ^ " " ^ output_dir) with
  | 0 ->
      print_endline
        ("Copied over media from " ^ contents_dir ^ " to " ^ output_dir)
  | _ ->
      print_endline
        ("Unable to copy over media from " ^ contents_dir ^ " to " ^ output_dir)

let build ~output_dir ~templates_dir ~contents_dir =
  let output_dir = Fpath.v output_dir in
  let templates_dir = Fpath.v templates_dir in
  let contents_dir = Fpath.v contents_dir in
  let _ =
    match Bos.OS.Path.must_exist contents_dir with
    | Ok _ -> ()
    | Error _ ->
        print_endline "ERROR the contents path does not exist." ;
        raise Exit
  in
  let _ = Bos.OS.Dir.create output_dir in
  build_pages ~output_dir ~templates_dir ~contents_dir ;
  build_blog ~output_dir ~templates_dir ~contents_dir ;
  build_comic ~output_dir ~templates_dir ~contents_dir ;
  copy_media
    ~output_dir:(Fpath.append output_dir (Fpath.v "media"))
    ~contents_dir:(Fpath.v "templates/media") ;
  copy_media
    ~output_dir:(Fpath.append output_dir (Fpath.v "media/blog"))
    ~contents_dir:(Fpath.append contents_dir (Fpath.v "blog/media")) ;
  copy_media
    ~output_dir:(Fpath.append output_dir (Fpath.v "media/comic"))
    ~contents_dir:(Fpath.append contents_dir (Fpath.v "comic/media"))
