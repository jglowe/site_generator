module Response = struct
  type t = {status : int; headers : (string * string) list; body : string}

  let of_string text =
    {status = 200; body = text; headers = [("Content-Type", "text/html")]}
end

module Request = struct
  type http_method =
    | Get
    | Post
    | Put
    | Head
    | Delete
    | Options
    | Trace
    | Connect
    | Patch
    | Other of string

  let http_method_to_string = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Head -> "HEAD"
    | Delete -> "DELETE"
    | Options -> "OPTIONS"
    | Trace -> "TRACE"
    | Connect -> "CONNECT"
    | Patch -> "PATCH"
    | Other s -> s

  let string_to_http_method = function
    | "GET" -> Get
    | "POST" -> Post
    | "PUT" -> Put
    | "HEAD" -> Head
    | "DELETE" -> Delete
    | "OPTIONS" -> Options
    | "TRACE" -> Trace
    | "CONNECT" -> Connect
    | "PATCH" -> Patch
    | s -> Other s

  type t = {
    uri : Uri.t;
    path : string;
    headers : (string * string) list;
    body : string;
    http_method : http_method;
    params : (string * string) list;
  }

  let add_header ~request ~key ~value =
    {request with headers = (key, value) :: request.headers}

  let get_url_param ~request ~key = Uri.get_query_param request.uri key

  let get_param ~request ~key = List.assoc_opt key request.params

  let create ~uri ~method_name ~path ~headers ~body ~params =
    let http_method = string_to_http_method method_name in
    {uri; path; headers; body; http_method; params}
end

let get_content_type path =
  let content_types =
    [ (Str.regexp ".+\\.html$", "text/html");
      (Str.regexp ".+\\.htm$", "text/html");
      (Str.regexp ".+\\.css$", "text/css");
      (Str.regexp ".+\\.png$", "image/png");
      (Str.regexp ".+\\.jpeg$", "image/jpeg");
      (Str.regexp ".+\\.jpg$", "image/jpg") ]
  in
  let match_content_type ctype (regex, content_type) =
    match Str.string_match regex (Fpath.to_string path) 0 with
    | true -> content_type
    | false -> ctype
  in
  List.fold_left match_content_type "text/html" content_types

let request_handler dir_to_serve (request : Request.t) =
  let open Bos.OS in
  let path =
    if String.length request.path > 1 then
      String.sub request.path 1 (String.length request.path - 1)
    else "index.html"
  in
  (* TODO -- Check that path is a subdirectory of dir_to_serve *)
  let path = Fpath.append (Fpath.v dir_to_serve) (Fpath.v path) in
  let not_found =
    ( 404,
      match
        Bos.OS.File.read
          (Fpath.append (Fpath.v dir_to_serve) (Fpath.v "404.html"))
      with
      | Ok v -> v
      | Error _ -> "404 Page not found" )
  in
  let code, response_body =
    match File.read path with
    | Ok file -> (200, file)
    | _ -> (
      match File.read (Fpath.v (Fpath.to_string path ^ ".html")) with
      | Ok file -> (200, file)
      | _ -> (
        match Dir.exists path with
        | Ok true -> (
          match File.read (Fpath.append path (Fpath.v "index.html")) with
          | Ok file -> (200, file)
          | _ -> not_found )
        | _ -> not_found ) )
  in
  let content_type_header = get_content_type path in
  print_endline (Fpath.to_string path) ;
  Response.
    {
      status = code;
      body = response_body;
      headers = [("Content-Type", content_type_header)];
    }

(*
 * Runs the web application using a cohttp_lwt_unix server using a given accept address and port.
 *
 * This should only be used for developing the site.
 *)
let serve to_serve_dir ~address ~port =
  let open Lwt in
  let server =
    let callback _conn req body =
      let uri = req |> Cohttp_lwt_unix.Request.uri in
      let path = Uri.path uri in
      let headers = Cohttp_lwt_unix.Request.headers req in
      let headers = Cohttp.Header.to_list headers in
      let method_name =
        req |> Cohttp_lwt_unix.Request.meth |> Cohttp.Code.string_of_method
      in
      let get_response (body : string) =
        let request =
          Request.create ~uri ~method_name ~body ~headers ~path
            ~params:[("", "")]
        in
        request_handler to_serve_dir request
      in
      let server_response (response : Response.t) =
        Cohttp_lwt_unix.Server.respond_string ~status:(`Code response.status)
          ~body:response.body
          ~headers:(Cohttp.Header.of_list response.headers)
          ()
      in
      body |> Cohttp_lwt.Body.to_string >|= get_response >>= server_response
    in
    let%lwt ctx = Conduit_lwt_unix.init ~src:address () in
    let ctx = Cohttp_lwt_unix.Client.custom_ctx ~ctx () in
    Cohttp_lwt_unix.Server.create ~ctx
      ~mode:(`TCP (`Port port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in
  (* Handles issue where server will crash if the client connection has an error: https://github.com/mirage/ocaml-cohttp/issues/511 *)
  (Lwt.async_exception_hook :=
     function
     | Unix.Unix_error (error, func, arg) ->
         Logs.warn (fun m ->
             m "Client connection error %s: %s(%S)" (Unix.error_message error)
               func arg)
     | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)) ;
  Lwt_main.run server
