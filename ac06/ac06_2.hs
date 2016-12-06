-- this is disastrously inefficient
import qualified Data.Map as Map
import Data.List.Split

data Instruction = Toggle (Int,Int) (Int,Int) | TurnOn (Int,Int) (Int,Int)| TurnOff (Int,Int) (Int,Int)
instance Show Instruction where
	show (Toggle from to) = "toggle " ++ (show from) ++ " through " ++ (show to) 
	show (TurnOn from to) = "turn on " ++ (show from) ++ " through " ++ (show to) 
	show (TurnOff from to) = "turn off " ++ (show from) ++ " through " ++ (show to) 

toggle :: Int -> Int
toggle = (+) 2

turnOff :: Int -> Int
turnOff light
	| light <= 0 = 0
	| otherwise = light - 1

turnOn :: Int -> Int
turnOn = (+) 1

doInstruction :: Instruction -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
doInstruction instruction houses =
	case instruction of
		Toggle (x1,y1) (x2,y2) -> foldr (Map.adjust toggle) houses [(x,y) | x<-[x1..x2],y<-[y1..y2]]
		TurnOn (x1,y1) (x2,y2) -> foldr (Map.adjust turnOn) houses [(x,y) | x<-[x1..x2],y<-[y1..y2]]
		TurnOff (x1,y1) (x2,y2) -> foldr (Map.adjust turnOff) houses [(x,y) | x<-[x1..x2],y<-[y1..y2]]

parseCoords :: String -> (Int, Int)
parseCoords string = (x, y)
	where [x, y] = map (\x -> read x :: Int) $ splitWhen (== ',') string

parseInstruction :: [String] -> Instruction
parseInstruction words =
	case words of 
	  "toggle":from:_:to:[] -> Toggle (parseCoords from) (parseCoords to)
	  "turn":"on":from:_:to:[] -> TurnOn (parseCoords from) (parseCoords to)
	  "turn":"off":from:_:to:[] -> TurnOff (parseCoords from) (parseCoords to)
	  _ -> error "bad instruction"

main :: IO ()
main = do 
    instructionInput <- readFile "input.txt"
    let instructionLines = (map words $ lines instructionInput)
        instructions = (map parseInstruction instructionLines)
        houses = foldl (\x -> \y -> doInstruction y x) (Map.fromList [((x,y),0)| x<-[0..999],y<-[0..999]]) instructions
    print $ foldl (\x -> \(_,light) -> x + light) 0 (Map.toList houses)