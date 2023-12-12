exception Parser_generator_failure

let rec parser_grammar (g : grammar) : string =
	parser_definition "let rec " (List.hd g) ^ "\n\n" ^
	List.fold_left (fun acc x -> acc ^ parser_definition "and " x ^ "\n\n") "" (List.tl g)

and parser_definition (start : string) (d : definition) : string =
	let func_name = String.lowercase_ascii @@ parser_identifier @@ fst d in
	start ^ func_name ^ " : stream -> " ^ func_name ^ " = fun s ->\n  " ^
	(parser_expression @@ snd d) ^ " s"

and parser_expression (e : expression) : string =
	let rec parser_expression_aux (f : sequence -> string) (l : sequence list) : string =
		match l with
		| [] -> ""
		| hd :: [] -> f hd
		| hd :: hd' :: [] -> "p_or (" ^ f hd ^ ") (" ^ f hd' ^ ")"
		| hd :: tl -> "p_or (" ^ f hd ^ ") (" ^ parser_expression_aux f tl ^ ")"
	in
	match snd e with
	| None -> parser_sequence @@ fst e
	| Some s -> parser_expression_aux (parser_sequence) (fst e :: s)

and parser_sequence (s : sequence) : string =
	match s with
	| None, _ -> raise Parser_generator_failure
	| Some s', None -> parser_prefix_list s'
	| Some s', Some w -> "(construct (fun x -> " ^ parser_identifier w ^ " x) (" ^ parser_prefix_list s' ^ "))"

and parser_prefix_list (p : prefix list) : string =
	let get_suffix_sym (_, s : prefix) : suffix_sym1 option =
		match s with
		| Sym (_, Some t) -> Some t
		| _ -> None
	in

	match p with
	| [] -> ""
	| hd :: [] -> parser_prefix hd
	| hd :: hd' :: [] ->
		begin match get_suffix_sym hd, get_suffix_sym hd' with
		| _, Some Minus -> "ignr (" ^ parser_prefix hd ^ ") (" ^ parser_prefix hd' ^ ")"
		| Some Minus, _ -> "ignl (" ^ parser_prefix hd ^ ") (" ^ parser_prefix hd' ^ ")"
		| _, _ -> "seq2 (" ^ parser_prefix hd ^ ") (" ^ parser_prefix hd' ^ ")" end
	| hd :: tl -> 
		begin match get_suffix_sym hd with
		| Some Minus -> "ignl (" ^ parser_prefix hd ^ ") (" ^ parser_prefix_list tl ^ ")"
		| _ -> "seq2 (" ^ parser_prefix hd ^ ") (" ^ parser_prefix_list tl ^ ")" end

and parser_prefix (sym, suffix : prefix) : string =
	match sym with
	| None -> parser_suffix suffix
	| Some And -> parser_suffix suffix
	| Some Not -> "p_not (" ^ parser_suffix suffix ^ ")"

and parser_suffix (s : suffix) : string =
	match s with
	| Sym (p, Some Q) -> "zero_or_one (" ^ parser_primary p ^ ")"
	| Sym (p, Some Star) -> "zero_or_more (" ^ parser_primary p ^ ")"
	| Sym (p, Some Plus) -> "one_or_more (" ^ parser_primary p ^ ")"
	| Sym (p, Some Minus) -> parser_primary p
	| Sym (p, None) -> parser_primary p
	| With (p, func) -> "apply_as " ^ parser_func func ^ " " ^ parser_primary p
	| As (ch, identifier) -> "p_as " ^ parser_identifier identifier ^ " (str_lit " ^ parser_ch ch ^ ")"
	| Prim p -> parser_primary p

and parser_primary (p : primary) : string =
	match p with
	| Match1 identifier -> String.lowercase_ascii @@ parser_identifier identifier
	| Match2 expression -> parser_expression expression
	| Match3 re -> parser_re re
	| Match4 ch -> "str_lit " ^ parser_ch ch

and parser_identifier (i : identifier) : string =
	i

and parser_func (f : func) : string =
	f

and parser_re (r : re) : string =
	let r' = String.mapi (fun i x -> if i < 3 || i = String.length r - 1 then Char.chr 0 else x) r in
	{|(regex ("^" ^ {||} ^ r' ^ "|}))"

and parser_ch (c : ch) : string =
	{|"|} ^ String.mapi (fun i x -> if i = 0 || i = String.length c - 1 then Char.chr 0 else x) c ^ {|"|}
