import Data.List
import Data.List.Split

ribbon_needed_sum :: String -> Integer
ribbon_needed_sum = 
    let boxStrings = splitWhen (== '\n')
        boxes = map (splitWhen (== 'x'))
        sides = map (map (read::String->Integer))
        ribbon = map ribbon_needed
    in
    foldl (+) 0 . ribbon . sides . boxes . boxStrings

ribbon_needed :: [Integer] -> Integer
ribbon_needed [l,w,h] =
    l*w*h + bow smallest
    where
        smallest = take 2 (sort [l,w,h])
        bow [s1,s2] = s1*2 + s2*2

main = do
    boxes <- readFile "input.txt"
    let sum = show (ribbon_needed_sum boxes)
    putStrLn sum