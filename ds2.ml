open Printf
open Unix

type file_tree = File of (string * Unix.stats)
               | Directory of (string * file_tree list)
               | Inaccessible of string

type cksum = { acc_size : int ; dirs : int ; files : int }
type dirsig = (string * cksum)

(*
 * Initially made for debugging
 *)

let rec _print_file_tree depth node = 
  for i = 0 to depth do print_char ' ' done;
  match node with
  | File (name, stat) -> printf "file: %s\n" name
  | Directory (name, nodes) -> 
      begin printf "dir: %s\n" name;
      List.iter (_print_file_tree (succ depth)) nodes end
  | Inaccessible _ -> ()

let print_file_tree tree = _print_file_tree 0 tree; tree

let empty_cksum = {acc_size = 0; dirs = 0; files = 0}

let list_directory path =
  Array.to_list @@ Sys.readdir path

let unix_type_of_file path =
  try 
    Some (Unix.lstat path).st_kind
  with Unix.Unix_error (e, s1, s2) ->
    eprintf "['%s'] %s : %s - %s\n" 
      (Filename.concat path (Sys.getcwd ()))
      (Unix.error_message e) s1 s1;
    None

let absolute_of_path path = Filename.concat (Sys.getcwd ()) path

let with_directory directory default func =
  let where = (Sys.getcwd ()) in
  let result = 
    try (Sys.chdir directory; func ())
    with Sys_error err -> (eprintf "%s\n" err; default)
  in
  Sys.chdir where;
  result

let rec _file_tree_of_directory () =
  list_directory "." 
  |> List.map file_tree_nodify

and file_tree_nodify ent = 
  match unix_type_of_file ent with
  | Some Unix.S_REG -> File (ent, Unix.stat ent)
  | Some Unix.S_DIR -> 
    Directory (
      absolute_of_path ent,
      with_directory ent [] _file_tree_of_directory
    ) 
  | None | _ -> Inaccessible ent

let file_tree_of_directory path =
  let make_dir_root stem = Directory (Sys.getcwd(), stem) in
  let recurse () = make_dir_root @@ _file_tree_of_directory () in
  with_directory path (make_dir_root []) recurse

let add_cksum a b = 
  { acc_size = a.acc_size + b.acc_size ;
    files = a.files + b.files ;
    dirs = a.dirs + b.dirs }

let rec _sum_file_tree cksum = function
  | File (name, stat) -> 
      { cksum with 
          acc_size = stat.st_size + cksum.acc_size;
          files = cksum.files + 1 }
  | Directory (name, nodes) -> 
      let d = { cksum with dirs = cksum.dirs + 1 } in
      let ds = (List.fold_left _sum_file_tree empty_cksum nodes) in
      add_cksum d ds
  | Inaccessible _ -> 
      cksum
      
let sum_file_tree tree = 
  match tree with
  | Directory _ -> List.fold_left _sum_file_tree empty_cksum [tree]
  | _ -> _sum_file_tree empty_cksum tree

let cksum_of_string d = sprintf 
  "size = %d, files = %d, dirs = %d" d.acc_size d.files d.dirs

let rec dirsig_list_of_tree tree =
  match tree with 
  | Directory (name, nodes) -> 
      let sum = sum_file_tree tree in
      (name, sum) :: (List.flatten @@ List.map dirsig_list_of_tree nodes)
  | File _ | Inaccessible _ -> []

let print_dirsig (name, cksum) = 
  printf "[%s] %s\n" (cksum_of_string cksum) name

let is_significant_dir s = 
  (snd s).acc_size > 1024*10 ||
  (snd s).files > 10 ||
  (snd s).dirs > 10

let find_duplicates needle haystack =
  let contains_needle straw = ((snd straw) = (snd needle)) in
  List.filter contains_needle haystack
  |> function | [] -> None | dups -> Some (needle :: dups)

let string_starts_with head snake =  
  String.sub snake 0 (String.length head) = head

let print_duplicates_block dups = 
  List.iter print_dirsig dups;
  print_newline ()

let dirsig_cksum_differs ds1 ds2 = (snd ds1) <> (snd ds2)
let dirsig_path_differs ds1 ds2 = not @@ string_starts_with (fst ds1) (fst ds2)

let rec create_duplicate_groups dirsigs : dirsig list list = 
  match dirsigs with
  | [] -> []
  | (head :: tail) ->
      match find_duplicates head tail with
      | None -> create_duplicate_groups tail
      | Some dups -> 
        dups ::
          (tail |> List.filter @@ dirsig_cksum_differs head
                |> List.filter @@ dirsig_path_differs head
                |> create_duplicate_groups)

let read_all_dirsigs path : dirsig list = 
  let by_path_length a b = 
    compare (String.length (fst a)) (String.length (fst b)) 
  in
  file_tree_of_directory path
  |> dirsig_list_of_tree
  |> List.filter is_significant_dir
  |> List.sort by_path_length

let _ = 
  let dirsigs = 
    try
      read_all_dirsigs Sys.argv.(1)
    with Invalid_argument _ -> 
      []
  in
  let by_dir_size a b = 
    compare (snd @@ List.hd a).acc_size 
            (snd @@ List.hd b).acc_size
  in
  create_duplicate_groups dirsigs
  |> List.sort by_dir_size
  |> List.iter print_duplicates_block
