count_parens :: String -> Int
count_parens =
	let calc_paren sum paren =
		case paren of 
			'(' -> sum + 1
			')' -> sum - 1
			_ -> error "Not a parenthesis"
	in
	foldl calc_paren 0

main = do
	parens <- readFile "input.txt"
	let count = show $ count_parens parens
	putStrLn count