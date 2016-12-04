import Data.List.Split

paper_needed_sum :: String -> Integer
paper_needed_sum = 
    let boxStrings = splitWhen (== '\n')
        boxes = map (splitWhen (== 'x'))
        sides = map (map (read::String->Integer))
        areas = map paper_needed
    in
    foldl (+) 0 . areas . sides . boxes . boxStrings

paper_needed :: [Integer] -> Integer
paper_needed [l, w, h] =
    2*s1 + 2*s2 + 2*s3 + smallest
    where
        s1 = l*w
        s2 = l*h
        s3 = w*h
        smallest = minimum [s1,s2,s3]

main = do
    boxes <- readFile "input.txt"
    let sum = show (paper_needed_sum boxes)
    putStrLn sum