(*
 *
 *
 *
 *)

type command = Positional of string | Flag of string | Option of string

type parsed_arguments = {
  positionals : string list;
  flags : (string * bool) list;
  options : (string * string) list;
}

(* let parse_arguments (command_list : command list) : parsed_arguments = *)
(*   let args = Array.to_list Sys.argv in *)
(*   let arguments = {positionals = []; flags = []; options = [];} in *)
(*   let parse_argument args *)
(*     match List.find_opt (fun a -> ) command_list with *)
(*     | Some flag -> *)
(*     | *)
(*   let rec parse_arguments args arguments = *)
(*     match args with *)
(*     | hd :: tl -> *)
(*     | [] -> arguments *)
(*   in *)
(*   parsed_arguments args arguments *)
