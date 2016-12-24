expand :: String -> String
expand [] = []
expand (x:xs) = 
    (show $ length elems) ++ [x] ++ (expand rest)
    where (elems, rest) = span (== x) (x:xs)

main :: IO ()
main = do
    start <- getLine
    rounds <- getLine
    let expansions = 1 + (read rounds :: Int)
    print $ length $ last $ take expansions $ iterate expand start