(*
 * Jonathan Lowe
 * 2019-07-28
 *
 * This file is the command line interface (thus the main executable) of the
 * static site generator. The various commands should be self explanitory.
 *
 *)

let build =
  Core.Command.basic ~summary:"Build the static site"
    ~readme:(fun () ->
      "Build the static site with the given templates, output, and contents \
       directories.")
    Core.Command.Let_syntax.(
      let%map_open output =
        flag "--output"
          (optional_with_default "output" string)
          ~doc:"Output directory. It defaults to output."
      and templates =
        flag "--templates"
          (optional_with_default "templates" string)
          ~doc:"Templates directory. It defaults to templates."
      and contents =
        flag "--contents"
          (optional_with_default "contents" string)
          ~doc:"Contents directory. It defaults to contents."
      in
      fun _ -> Build.build ~output ~templates ~contents)

let serve =
  Core.Command.basic ~summary:"Serve the static site"
    ~readme:(fun () ->
      "Serve the static site on a given host address on a specific port. This \
       should only be used for developing and testing the site locally.")
    Core.Command.Let_syntax.(
      let%map_open to_serve_dir =
        anon (maybe_with_default "output" ("output" %: string))
      and host =
        flag "--host"
          (optional_with_default "127.0.0.1" string)
          ~doc:"Address to accept connections from."
      and port =
        flag "-p"
          (optional_with_default 8080 int)
          ~doc:"Port to serve the static site on. It defaults to 8080."
      in
      fun _ -> Serve.serve to_serve_dir ~address:host ~port)

let clean =
  Core.Command.basic ~summary:"Remove the built static site"
    ~readme:(fun () -> "Remove the build artifacts of generating the site.")
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

let _ = Core.Command.run ~version:"0.1" ~build_info:"Initial Prototype" command
