let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let () =
	let grammar = read_whole_file "grammar" in
	let s = new stream in
	s#read grammar ;
	let ast = grammar s in
	let gast = ast_grammar ast in
	let oc = open_out "build/ast.ml" in
	Printf.fprintf oc "%s\n" gast ; close_out oc ;
	let gparse = ast_parser ast in
	let oc = open_out "build/parser.ml" in
	Printf.fprintf oc "%s\n" gparse ; close_out oc ;