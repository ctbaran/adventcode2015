import Data.Tuple

parseEscape :: Integer -> Integer -> String -> (Integer, Integer)
parseEscape code memory (x:xs) =
	case x of
	 '\\' -> parseString (code+1) (memory+1) xs
	 '"' -> parseString (code+1) (memory+1) xs
	 'x' -> parseString (code+1) (memory+3) (drop 2 xs)
	 _ -> error "bad escape"

parseString :: Integer -> Integer -> String -> (Integer, Integer)
parseString code memory string
	| null string = (code, memory)
	| otherwise = 
		case x of
		 '\\' -> parseEscape code (memory+1) xs
		 '"' -> parseString code (memory+1) xs
		 _ -> parseString (code+1) (memory+1) xs
		 where x:xs = string

encodeString :: String -> String
encodeString string =
	case string of
		'\\':'"':xs -> '\\':'\\':'\\':'"':(encodeString xs)
		'\\':'x':xs -> '\\':'\\':'x':(encodeString xs)
		'\\':'\\':xs -> '\\':'\\':'\\':'\\':(encodeString xs)
		'"':xs -> '"':'\\':'"':(encodeString xs)
		x:xs -> x:(encodeString xs)
		[] -> []

main :: IO ()
main = do 
    strings <- readFile "input.txt"
    let add_pairs (i, j) (k, l) = (i + k, j + l)
    	original_code = snd . foldl (add_pairs) (0, 0) $ map (parseString 0 0) $ lines strings
    	encoded_code = snd . foldl (add_pairs) (0, 0) $ map (parseString 0 0) $ map encodeString $ lines strings
    print $ encoded_code - original_code