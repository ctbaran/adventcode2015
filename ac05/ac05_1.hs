import Text.Regex.PCRE

-- just do this instead of importing something that handles backreferences
repeating :: Char -> String -> Bool
repeating previous string
	| null string = False
	| previous == head string = True
	| otherwise = repeating (head string) (tail string)

enough_vowels :: String -> Bool
enough_vowels string =
	if length matches >= 3 
		then True
	else
		False
	where matches = getAllTextMatches $ string =~ "(a|e|i|o|u)" :: [String]

badstring :: String -> Bool
badstring string =
	case getAllTextMatches $ string =~ "(ab|cd|pq|xy)" :: [String] of
		[] -> True
		_ -> False

nicestrings :: [String] -> [String]
nicestrings = 
	filter (\x -> repeating (head x) (tail x)) . filter enough_vowels . filter badstring

main :: IO ()
main = do 
    strings <- readFile "input.txt"
    print $ (length $ nicestrings (lines strings))