let rec grammar : stream -> grammar = fun s ->
	print_endline @@ "Grammar: " ^ s#string ;
	one_or_more (ignr definition (str_lit "\n")) s

and definition : stream -> definition = fun s ->
	print_endline @@ "Definition: " ^ s#string ;
	seq2 identifier (ignl (str_lit " <- ") expression) s

and expression : stream -> expression = fun s ->
	print_endline @@ "Expression: " ^ s#string ;
	seq2 sequence (zero_or_more (ignl (seq2 (zero_or_more (str_lit " ")) (str_lit "| ")) sequence)) s

and sequence : stream -> sequence = fun s ->
	print_endline @@ "Sequence: " ^ s#string ;
	seq2 (zero_or_more (ignr prefix (zero_or_more (str_lit " ")))) (zero_or_one (ignl (str_lit "case ") identifier)) s

and prefix : stream -> prefix = fun s ->
	print_endline @@ "Prefix: " ^ s#string ;
	seq2 (zero_or_one (p_or (p_as And (str_lit "&")) (p_as Not (str_lit "!")))) suffix s

and suffix : stream -> suffix = fun s ->
	print_endline @@ "Suffix: " ^ s#string ;
	p_or (construct (fun x -> With x) 
				    (seq2 primary 
				    	  (ignl (str_lit " with ") 
				    	        func
				    	  )
				    )
		 ) 
		 (p_or (construct (fun x -> As x) 
		 				  (seq2 ch 
		 				  	    (ignl (str_lit " as ") 
		 				  	          identifier
		 				  	    )
		 				  )
 			   ) 
		 	   (p_or (construct (fun x -> Sym x) 
		 	   			  (seq2 primary 
		 	   			  	    (zero_or_one (p_or (p_as Q 
		 	   			  	    	                     (str_lit "?")
		 	   			  	    	                ) 
		 	   			  	                        (p_or (p_as Star 
		 	   			  	                        	        (str_lit "*")
		 	   			  	                        	   ) 
		 	   											   (p_or (p_as Plus 
		 	   											   	           (str_lit "+")
		 	   											   	     ) 
		 	   												     (p_as Minus 
		 	   												     	   (str_lit "-")
		 	   												     )
		 	   											   )
		 	   										)
		 	   			  	                 )
		 	   			  	    )
		 	   			  )
		 	   )
		 	   (construct (fun x -> Prim x) primary)
		 )) s

and primary : stream -> primary = fun s ->
	print_endline @@ "Primary: " ^ s#string ;
	p_or (construct (fun x -> Match1 x) (ignr identifier (p_not (str_lit " <- ")))) (p_or (construct (fun x -> Match2 x) (ignl (str_lit "(") (ignr expression (str_lit ")")))) (p_or (construct (fun x -> Match3 x) re) (construct (fun x -> Match4 x) ch))) s

and identifier : stream -> identifier = fun s ->
	print_endline @@ "Identifier: " ^ s#string ;
	regex ("^" ^ {|[A-Z][a-z0-9]*|}) s

and func : stream -> identifier = fun s ->
	print_endline @@ "Func: " ^ s#string ;
	regex ("^" ^ {|[\\][A-Z]+|}) s

and re : stream -> re = fun s ->
	print_endline @@ "Re: " ^ s#string ;
	regex ("^" ^ {|re\((.*)\)|}) s

and ch : stream -> ch = fun s ->
	print_endline @@ "Ch: " ^ s#string ;
	regex ("^" ^ {|'[^']*'|}) s