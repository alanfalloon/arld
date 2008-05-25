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
let pp_l pp_elt fmt = function
    [] -> ()
  | e1::en ->
      pp fmt "@[<hov>";
      pp fmt "%a" pp_elt e1;
      List.iter (pp fmt ",@ %a" pp_elt) en;
      pp fmt "@]"
let pp_a pp_elt fmt a = pp_l pp_elt fmt (Array.to_list a)
let pp_ls pp_elt fmt = function
    [] -> ()
  | e1::en ->
      pp fmt "@[<hov>";
      pp fmt "%a" pp_elt e1;
      List.iter (pp fmt "@ %a" pp_elt) en;
      pp fmt "@]"
let pp_as pp_elt fmt a = pp_ls pp_elt fmt (Array.to_list a)
let pp_S fmt = pp fmt "%S"
let pp_s = Format.pp_print_string
let pp_pair pp1 pp2 fmt (e1,e2) =
  pp fmt "(%a,@ %a)" pp1 e1 pp2 e2

let failprintf fmt =
  let b = Buffer.create 3 in
  let f = Format.formatter_of_buffer b in
  let fail _ =
    failwith (Buffer.contents b)
  in
  Format.kfprintf fail f ("@["^^fmt^^"@]@.")

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

let absolute_name fn =
  if Filename.is_relative fn
  then Filename.concat (Sys.getcwd()) fn
  else fn

let find (f,paths) =
  match f with
      Obj fn ->
        let fn = absolute_name fn in
        if Sys.file_exists fn then Obj fn
        else failprintf "cannot find %S" fn
    | Lib n ->
        let fn = Format.sprintf "lib%s.a" n in
        let paths = List.rev paths in
        let fns = List.map (fun d -> Filename.concat d fn) paths in
        let fn =
          try List.find Sys.file_exists fns
          with Not_found -> failprintf "cannot find %S in %a" fn (pp_l pp_s) paths
        in
        let fn = absolute_name fn in
        Lib fn
;;

let sys cwd args =
  let pid = Unix.fork () in
  if 0 = pid then begin
    let () = Sys.chdir cwd in
    Unix.execvp args.(0) args
  end else begin
    let _, s = Unix.waitpid [] pid in
    match s with
        Unix.WEXITED 0 -> ()
      | Unix.WEXITED x -> failprintf "'%a' exited with status %d" (pp_a pp_s) args x
      | Unix.WSIGNALED x -> failprintf "'%a' killed with signal %d" (pp_a pp_s) args x
      | Unix.WSTOPPED _ -> assert false
  end

let main () = try
  let argv = Sys.argv in
  let verbose,output,files = parse_args argv in
  let debug lv fmt =
    if lv <= verbose
    then Format.printf ("@["^^fmt^^"@]@.")
    else Format.ifprintf Format.std_formatter fmt
  in
  let () = debug 3 "v:%d@ o:%S@ fs:%a" verbose output (pp_list (pp_pair pp_file (pp_list pp_S))) files in
  let files = List.map find files in
  let () = debug 3 "files:@ %a" (pp_list pp_file) files in
  let tmpd =
    let rec mkt i =
      try
        let d = Format.sprintf "arln%03d" i in
        let d = Filename.concat Filename.temp_dir_name d in
        let () = Unix.mkdir d 0o700 in
        d
      with Unix.Unix_error (Unix.EEXIST,_,_) -> mkt (i+1)
    in
    mkt 0
  in
  let () = debug 2 "temp dir %S" tmpd in
  let sys cwd cmd =
    debug 1 "%20s: %a" cwd (pp_as pp_s) cmd;
    sys cwd cmd
  in
  let add_to_tmp tmp = function
      Obj f -> sys (Sys.getcwd()) [|"cp";f;tmp|]
    | Lib f -> sys tmp [|"ar";"x";f|]
  in
  let () = List.iter (add_to_tmp tmpd) files in
  let ofiles = Sys.readdir tmpd in
  let ofiles = Array.map (Filename.concat tmpd) ofiles in
  let cmd = Array.append [|"ar";"crs";output|] ofiles in
  let () = sys (Sys.getcwd()) cmd in
  let () = sys (Sys.getcwd()) [|"rm";"-rf";tmpd|] in
  ()
with Failure msg ->
  Format.printf "@[%s@]@." msg;
  exit 1

let () = main ()
