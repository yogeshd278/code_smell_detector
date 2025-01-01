open Printf

let read_lines file =
  let ic = open_in file in
  let rec read_all acc =
    try
      let line = input_line ic in
      read_all (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_all []

let detect_long_lines lines max_length =
  List.mapi (fun i line -> (i + 1, String.length line, line)) lines
  |> List.filter (fun (_, len, _) -> len > max_length)

let detect_nested_ifs lines =
  let rec aux nesting_level acc line_num = function
    | [] -> List.rev acc
    | line :: rest ->
        let nested =
          (String.contains line '{' && String.contains line 'i') &&
          String.contains line 'f' && nesting_level > 2
        in
        let new_level = nesting_level + (if String.contains line '{' then 1 else 0) -
                                      (if String.contains line '}' then 1 else 0) in
        let acc = if nested then (line_num, line) :: acc else acc in
        aux new_level acc (line_num + 1) rest
  in
  aux 0 [] 1 lines

let analyze file =
  let lines = read_lines file in
  let long_lines = detect_long_lines lines 80 in
  let nested_ifs = detect_nested_ifs lines in
  
  printf "Long lines:\n";
  List.iter (fun (line_num, len, line) ->
    printf "  Line %d: %s (%d characters)\n" line_num line len
  ) long_lines;
  
  printf "\nNested ifs:\n";
  List.iter (fun (line_num, line) ->
    printf "  Line %d: %s\n" line_num line
  ) nested_ifs

let () =
  if Array.length Sys.argv <> 2 then
    printf "Usage: %s <file.js>\n" Sys.argv.(0)
  else
    let file = Sys.argv.(1) in
    analyze file
