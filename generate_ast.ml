exception Token of string

let rec fold_option (f : string -> 'a -> string) (acc : string) (l : 'a list option) : string =
	match l with
	| None -> acc
	| Some l' -> 
		begin match l' with
		| hd :: tl -> 
			begin try fold_option f (f acc hd) (Some tl) with
			| Token s -> s end
		| [] -> acc
		end

let fold_option_sep (f : 'a -> string) (sep : string) (acc : string) (l : 'a list option) : string =
	let sep_aux acc x =
		let res = f x in
		if res = "" then
			acc
		else if acc = "" then
			res
		else
			acc ^ sep ^ res
	in
	match l with
	| None -> acc
	| Some l' ->
		begin match l' with
		| [] -> acc
		| hd :: tl -> List.fold_left sep_aux (acc ^ (f hd)) tl end

module SS = Set.Make(String)

class tokens =
object(self)
	val mutable s = SS.empty

	method add (str : string) =
		s <- SS.add str s

	method print : string =
		if SS.is_empty s then
			""
		else
			(fold_option_sep (fun x -> x) " | " "type token = " (Some (SS.elements s))) ^ " \n"
end

let tokens = new tokens

let rec ast_grammar (a : grammar) : string = 
	tokens#print ^
	ast_definition "type " (List.hd a) ^ " \n" ^
	List.fold_left (fun acc x -> acc ^ x) "" (List.map (fun x -> ast_definition "and " x ^ " \n") (List.tl a))

and ast_definition (dec : string) (d : definition) : string =
	dec ^ ast_identifier (fst d) ^ " = " ^ ast_expression (snd d)

and ast_expression (e : expression) : string =
	try ast_sequence (fst e) ^ fold_option (fun acc x -> acc ^ " | " ^ (ast_sequence x)) "" (snd e) with
	| Token s -> raise (Token s)

and ast_sequence (s : sequence): string =
	try match snd s with
	| None -> fold_option_sep (ast_prefix) " * " "" (fst s)
	| Some i -> i ^ " of " ^ fold_option_sep (ast_prefix) " * " "" (fst s)
	with Token s -> raise (Token s)

and ast_prefix (p : prefix) : string =
	try match fst p with
	| None -> ast_suffix (snd p)
	| Some p' -> 
		begin match p' with
		| And -> ast_suffix (snd p)
		| Not -> "" end
	with Token s -> raise (Token s)

and ast_suffix (s : suffix) : string =
	match s with
	| With (primary, func) -> ast_func func
	| As (ch, identifier) -> tokens#add identifier ; raise (Token "token")
	| Sym (primary, sym) ->
		begin match sym with
		| None -> ast_primary primary
		| Some Q -> ast_primary primary ^ " option"
		| Some Star -> ast_primary primary ^ " list option"
		| Some Plus -> ast_primary primary ^ " list"
		| Some Minus -> "" end
	| Prim primary -> ast_primary primary

and ast_primary (p : primary) : string =
	try match p with
	| Match1 identifier -> ast_identifier identifier
	| Match2 expression -> "(" ^ ast_expression expression ^ ")"
	| Match3 re -> ast_re re
	| Match4 ch -> ast_ch ch
	with Token s -> s

and ast_identifier (i : identifier) : string =
	String.lowercase_ascii i

and ast_func (f : string) : string =
	match f with
	| {|\INT|} -> "int"
	| _ -> raise (Invalid_argument f)

and ast_re (_ : string) : string =
	"string"

and ast_ch (_ : string) : string =
	"string"