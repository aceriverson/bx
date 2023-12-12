type prefix_sym1 = And | Not
type suffix_sym1 = Q | Star | Plus | Minus

type grammar = definition list
and definition = identifier * expression
and expression = sequence * sequence list option
and sequence = prefix list option * identifier option
and prefix = prefix_sym1 option * suffix
and suffix = With of (primary * func) | As of (ch * identifier) | Sym of (primary * suffix_sym1 option) | Prim of primary
and primary = Match1 of identifier | Match2 of expression | Match3 of re | Match4 of ch
and identifier = string
and func = string
and re = string
and ch = string