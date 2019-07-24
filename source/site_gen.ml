(*
 *
 *
 *
 *
 *
 *)

let build =
  Core.Command.basic ~summary:"Build the static site"
    Core.Command.Let_syntax.(
      let%map_open output =
        flag "--output"
          (optional_with_default "output" string)
          ~doc:"output directory"
      and templates =
        flag "--templates"
          (optional_with_default "templates" string)
          ~doc:"templates directory"
      and contents =
        flag "--contents"
          (optional_with_default "contents" string)
          ~doc:"contents directory"
      in
      fun _ -> Build.build ~output ~templates ~contents)

let serve =
  Core.Command.basic ~summary:"Serve the static site"
    Core.Command.Let_syntax.(
      let%map_open to_serve_dir =
        anon (maybe_with_default "output" ("output" %: string))
      and host =
        flag "--host" (optional_with_default "127.0.0.1" string) ~doc:"Host"
      and port =
        flag "-p"
          (optional_with_default 8080 int)
          ~doc:"Port to serve the static site on. It defaults to 8080."
      in
      fun _ -> Serve.serve to_serve_dir ~address:host ~port)

let clean =
  Core.Command.basic ~summary:"Remove the built static site"
    Core.Command.Let_syntax.(
      let%map_open output_dir =
        anon (maybe_with_default "output" ("output" %: string))
      in
      fun _ ->
        match Sys.command ("rm -r " ^ output_dir) with
        | 0 -> print_endline "Cleaned"
        | _ -> print_endline ("Unable to remove " ^ output_dir))

let command =
  Core.Command.group ~summary:"Build and serve a static site."
    [("build", build); ("serve", serve); ("clean", clean)]

let _ = Core.Command.run ~version:"1.0" ~build_info:"RWO" command
