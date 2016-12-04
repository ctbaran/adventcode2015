enter_basement :: Int -> Int -> String -> Int
enter_basement position sum parens
	| sum == -1 = position
	| null parens = error "Never entered basement"
	| otherwise = 
	let calc_paren paren =
		case paren of 
			'(' -> 1
			')' -> -1
			_ -> error "Not a parenthesis"
	in
	case sum of
		-1 -> position
		_ -> enter_basement (1 + position) (sum + calc_paren paren) rest
	where paren:rest = parens

main = do
	parens <- readFile "input.txt"
	let basement = show $ enter_basement 0 0 parens
	putStrLn basement