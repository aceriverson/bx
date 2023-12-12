module Print =
struct
	exception Print_error

	let pp_list (a : 'a -> string) (p : 'a list) : string =
		List.fold_left (fun acc p' -> acc ^ a p') "" p

	let pp_option (a : 'a -> string) (p : 'a option) : string =
		match p with
		| Some p' -> a p'
		| None -> ""

	let pp_seq2 (a : 'a -> string) (b : 'b -> string) (p : 'a * 'b) : string =
		a (fst p) ^ b (snd p)
end

module Parse =
struct
	exception Parse_error

	class stream = 
	object(self)
		val mutable s = ([] : char list)

		method read (str : string) =
			s <- s @ List.init (String.length str) (String.get str)

		method throw_if_empty =
			match s with
			| [] -> raise End_of_file
			| _ -> ()

		method reset =
			s <- []

		method string =
			String.of_seq (List.to_seq s)

		method get =
			match s with
			| hd :: tl -> s <- tl ; hd
			| [] -> raise End_of_file

		method peek = 
			match s with
			| hd :: _ -> hd
			| [] -> raise End_of_file

		method return (cs : char list) =
			s <- (List.rev cs) @ s
	end

	let apply_as (f : 'a -> 'b) (a : stream -> 'a) (s : stream) : 'b =
		print_endline @@ "Apply_as: " ^ s#string ;
		f (a s)

	let construct (c : 'a -> 'b) (a : stream -> 'a) (s : stream) : 'b =
		print_endline @@ "Construct: " ^ s#string ;
		c (a s)

	let ignl (a : stream -> 'a) (b : stream -> 'b) (s : stream) : 'b =
		print_endline @@ "Ignl: " ^ s#string ;
		let _ = a s in b s

	let ignr (a : stream -> 'a) (b : stream -> 'b) (s : stream) : 'a =
		print_endline @@ "Ignr: " ^ s#string ;
		let res = a s in
		let _ = b s in
		res

	let p_as (a : 'a) (b : stream -> 'b) (s : stream) : 'a =
		print_endline @@ "P_as: " ^ s#string ;
		let _ = b s in a

	let p_not (a : stream -> 'a) (s : stream) : unit =
		print_endline @@ "P_not: " ^ s#string ;
		let s_copy = s#string in
		try
		    ignore (a s);
		    raise Parse_error
  		with
  		| End_of_file | Parse_error -> s#reset ; s#read s_copy ; ()

	let p_or (a : stream -> 'a) (b : stream -> 'a) (s : stream) : 'a = 
		print_endline @@ "P_or: " ^ s#string ;
		let s_copy = s#string in
		try a s with
		| _ -> s#reset ; s#read s_copy ; b s

	let regex (str : string) (s : stream) : string =
		print_endline @@ "Regex: " ^ s#string ;
		let r = Re.Str.regexp str in
		let s_copy = s#string in
		s#throw_if_empty ; s#reset ;
		if Re.Str.string_match r s_copy 0 then begin
			let new_start = Re.Str.match_end () in
			let res = Re.Str.matched_string s_copy in
			s#read @@ Re.Str.string_after s_copy new_start ;
			res
		end else begin
			s#read s_copy ; raise Parse_error
		end
(* 		let r = Re.Str.regexp str in
		let stream_string = s#string in
		s#throw_if_empty ; s#reset ;
		match Re.Str.full_split r stream_string with
		| Delim hd :: Text tl :: [] -> s#read tl ; hd
		| Delim hd :: [] -> hd
		| [] | _ -> s#read stream_string ; raise Parse_error *)

	let seq2 (a : stream -> 'a) (b : stream -> 'b) (s : stream) : 'a * 'b =
		print_endline @@ "Seq2: " ^ s#string ;
		let a' = a s in
		let b' = b s in
		a', b'

	let str_lit (str : string) (s : stream) : string =
		print_endline @@ "Str_lit: " ^ s#string ^ " and checking " ^ str ;
		let str_as_list = List.init (String.length str) (String.get str) in
		let rec str_lit_aux (acc : char list) (cs : char list) (s : stream) : unit =
			match cs with
			| hd :: tl -> 
				if hd = s#peek then begin
					str_lit_aux (s#get :: acc) tl s 
				end else begin
					s#return acc ; raise Parse_error
				end
			| [] -> () in
		str_lit_aux [] str_as_list s ; str

	let zero_or_more (a : stream -> 'a) (s : stream) : ('a list) option =
		print_endline @@ "Zero_or_more: " ^ s#string ;
		let rec zero_or_more_aux (a : stream -> 'a) (s : stream) : 'a list =
			let res = try
		      	Some (a s)
		    with
		    | End_of_file | Parse_error -> None in
		    match res with
		    | Some r -> r :: zero_or_more_aux a s
		    | None -> [] in
		match zero_or_more_aux a s with
		| [] -> None
		| res -> Some res

	let one_or_more (a : stream -> 'a) (s : stream) : 'a list =
		print_endline @@ "One_or_more: " ^ s#string ;
		match zero_or_more a s with
		| Some res -> res
		| None -> raise Parse_error

	let zero_or_one (a : stream -> 'a) (s : stream) : 'a option =
		print_endline @@ "Zero_or_one: " ^ s#string ;
		try Some (a s) with
		| End_of_file | Parse_error -> None
end
