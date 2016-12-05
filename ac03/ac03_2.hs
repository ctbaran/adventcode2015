import Data.Set

data Mover = Santa | RoboSanta

deliver_presents :: String -> Mover -> (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Int
deliver_presents [] _ (x1, y1) (x2, y2) visited = size (insert (x2, y2) $ insert (x1, y1) visited)
deliver_presents "\n" _ (x1, y1) (x2, y2) visited = size (insert (x2, y2) $ insert (x1, y1) visited)
deliver_presents (dir:directions) mover (x1, y1) (x2, y2) visited =
	case mover of 
		Santa -> deliver_presents directions RoboSanta newCoords (x2, y2) (insert (x1, y1) visited)
				 where newCoords = move_dir dir (x1, y1)
		RoboSanta -> deliver_presents directions Santa (x1, y1) newCoords (insert (x2, y2) visited)
				 	 where newCoords = move_dir dir (x2, y2) 

move_dir :: Char -> (Int, Int) -> (Int, Int)
move_dir dir (x, y) =
	case dir of
		'^' -> (x,y+1)
		'v' -> (x,y-1)
		'>'	-> (x+1,y)
		'<'	-> (x-1,y)
		_ -> error "Bad direction"

main = do
    directions <- readFile "input.txt"
    let visited = show (deliver_presents directions Santa (0, 0) (0, 0) empty)
    putStrLn visited
