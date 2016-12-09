import Data.Char
import Data.List

nextChar :: Char -> Char
nextChar c = chr (ord c + 1)

nextPassword :: String -> String
nextPassword current =
    case last current of
        'z' -> (nextPassword allButLast) ++ "a"
        char -> allButLast ++ [nextChar char]
    where allButLast = take ((length current) - 1) current

straight :: String -> Bool
straight [] = False
straight (x:xs) = 
    case isPrefixOf [y,z] xs of
        True -> True
        False -> straight xs
    where y = nextChar x
          z = nextChar y

badLetters :: String -> Bool
badLetters password = (notElem 'i' password) && (notElem 'o' password) && (notElem 'l' password)

twoPairs :: String -> Maybe Char -> Bool
twoPairs [] _ = False
twoPairs (x1:x2:xs) Nothing 
        | x1 == x2 = twoPairs xs (Just x1)
        | otherwise = twoPairs (x2:xs) Nothing
twoPairs (y1:y2:ys) (Just x)
        | ((x /= y1) && (y1 == y2)) = True
        | otherwise = twoPairs (y2:ys) (Just x)
twoPairs (_:xs) char = twoPairs xs char

goodPassword :: String -> Bool
goodPassword password = (badLetters password) && (straight password) && (twoPairs password Nothing)

main :: IO ()
main = do 
    password <- getLine
    let passwords = iterate nextPassword password
    print $ take 2 $ filter goodPassword passwords