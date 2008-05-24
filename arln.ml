type file =
    Obj of string
  | Lib of string

let pp = Format.fprintf
let pp_file fmt = function
    Obj s -> pp fmt "@[Obj@ %S@]" s
  | Lib s -> pp fmt "@[Lib@ %S@]" s
let pp_list pp_elt fmt = function
    [] -> pp fmt "[]"
  | e1::en ->
      pp fmt "@[<hov>[@,";
      pp fmt "%a" pp_elt e1;
      List.iter (pp fmt ";@ %a" pp_elt) en;
      pp fmt "@,]@]"
let pp_s fmt = pp fmt "%S"
let pp_pair pp1 pp2 fmt (e1,e2) =
  pp fmt "(%a,@ %a)" pp1 e1 pp2 e2

let logger lv =
  let msg lv' fmt =
    if lv' <= lv
    then Format.printf ("@["^^fmt^^"@]@.")
    else Format.ifprintf Format.std_formatter fmt
  in
  msg

let failprintf fmt =
  Format.ksprintf failwith fmt

let default_paths = ["/lib"; "/usr/lib"]

let usage =
  "arln -o <output lib> [-v] [-Lpath|-llib|obj]*
Read all the object files and libraries and create a new library
containing all the named objects and contents of all the named
libraries"

let parse_args argv =
  let files = ref []
  and search_paths = ref default_paths
  and verbose = ref 0
  and output = ref None in
  let add_lib lname =
    files := (Lib lname, !search_paths) :: !files
  and add_path path =
    search_paths := path :: !search_paths
  and add_obj o =
    files := (Obj o, !search_paths) :: !files
  and increase_verb () =
    incr verbose
  and set_output o =
    match !output with
        None -> output := Some o
      | Some _ -> failprintf "can only set -o once"
  in
  let i = ref 1 in
  let next o =
    incr i;
    if !i < Array.length argv && '-' <> argv.(!i).[0]
    then argv.(!i)
    else failprintf "Expected another argument after %s" o
  in
  let flag_of s =
    try String.sub s 0 2
    with Invalid_argument _ -> ""
  in
  let rest_of s =
    String.sub s 2 (String.length s - 2)
  in
  while !i < Array.length argv do
    let arg = argv.(!i) in
    let () = match arg, flag_of arg with
        "-o",_ -> set_output (next "-o")
      | "-l",_ -> add_lib (next "-l")
      | "-L",_ -> add_path (next "-L")
      | "-v",_ -> increase_verb ()
      | _,"-o" -> set_output (rest_of arg)
      | _,"-l" -> add_lib (rest_of arg)
      | _,"-L" -> add_path (rest_of arg)
      | _,_ -> add_obj arg
    in
    incr i
  done;
  let output = match !output with
      None -> failprintf "an output file must be specified with -o"
    | Some o -> o
  in
  (!verbose,output,!files)
;;

let main () =
  let argv = Sys.argv in
  let verbose,output,files = parse_args argv in
  let debug = logger verbose in
  let () = debug 1 "v:%d@ o:%S@ fs:%a" verbose output (pp_list (pp_pair pp_file (pp_list pp_s))) files in
  ()

let () = main ()
