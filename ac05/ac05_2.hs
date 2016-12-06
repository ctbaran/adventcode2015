import Data.List

repeating :: Char -> String -> Bool
repeating char string
	| null string = False
	| otherwise = 
		case string of 
			(x:y:ys) -> char == y
			_ -> False

repeating_window :: String -> Bool
repeating_window string
	| null string = False
	| otherwise = 
		case repeating (head string) (drop 1 string) of
			True -> True
			False -> repeating_window (tail string)

infix_window :: String -> Bool
infix_window string
	| null string = False
	| otherwise =
		case infix_sub (take 2 string) (drop 2 string) of
			True -> True
			False -> infix_window (drop 1 string)
		where infix_sub sub string = isInfixOf sub string

nicestrings :: [String] -> [String]
nicestrings = 
	filter repeating_window . filter infix_window

main :: IO ()
main = do 
    strings <- readFile "input.txt"
    print $ (length $ nicestrings (lines strings))