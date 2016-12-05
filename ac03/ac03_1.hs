import Data.Set

deliver_presents :: String -> (Int, Int) -> Set (Int, Int) -> Int
deliver_presents [] (x, y) visited = size (insert (x, y) visited)
deliver_presents "\n" (x, y) visited = size (insert (x, y) visited)
deliver_presents (dir:directions) (x, y) visited =
	case dir of
		'^' -> deliver_presents directions (x, y+1) (insert (x, y) visited)
		'v' -> deliver_presents directions (x, y-1) (insert (x, y) visited)
		'>'	-> deliver_presents directions (x+1, y) (insert (x, y) visited)
		'<'	-> deliver_presents directions (x-1, y) (insert (x, y) visited)
		_ -> error "Bad direction"

main = do
    directions <- readFile "input.txt"
    let visited = show (deliver_presents directions (0, 0) empty)
    putStrLn visited
