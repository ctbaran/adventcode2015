import Data.Word
import Data.Bits
import qualified Data.Map as Map
import Text.Read

type Wire = Either String Word16
data Gate = IN Wire |
            AND Wire Wire |
            OR Wire Wire |
            NOT Wire |
            LSHIFT Wire Int |
            RSHIFT Wire Int 

instance Show Gate where
    show (IN in1) = (show in1) ++ " -> " 
    show (NOT in1) = "NOT " ++ (show in1) ++ " -> " 
    show (AND in1 in2) = (show in1) ++ " AND " ++ (show in2) ++ " -> " 
    show (OR in1 in2) = (show in1) ++ " OR " ++ (show in2) ++ " -> "
    show (LSHIFT in1 shift) = (show in1) ++ " LSHIFT " ++ (show shift) ++ " -> " 
    show (RSHIFT in1 shift) = (show in1) ++ " RSHIFT " ++ (show shift) ++ " -> "

readWord16 :: String -> Either String Word16
readWord16 string =
    case readMaybe string :: Maybe Word16 of
        Just int -> Right int
        Nothing -> Left string

parseGate :: [String] -> (String, (Maybe Word16, Gate))
parseGate words =
    case words of
        in1:_:out:[] -> (out, (readMaybe in1 :: (Maybe Word16), IN (readWord16 in1))) 
        "NOT":in1:_:out:[] -> (out, (Nothing, NOT (readWord16 in1)))
        in1:"AND":in2:_:out:[] -> (out, (Nothing, AND (readWord16 in1) (readWord16 in2)))
        in1:"OR":in2:_:out:[] -> (out, (Nothing, OR (readWord16 in1) (readWord16 in2)))
        in1:"LSHIFT":shift:_:out:[] -> (out, (Nothing, LSHIFT (readWord16 in1) (read shift :: Int)))
        in1:"RSHIFT":shift:_:out:[] -> (out, (Nothing, RSHIFT (readWord16 in1) (read shift :: Int)))
        _ -> error "Bad gate descriptor"

lookupLabel :: Map.Map String (Maybe Word16, Gate) -> String -> (Word16, (Map.Map String (Maybe Word16, Gate)))
lookupLabel circuit label =
    case Map.lookup label circuit of
        Just (Just val, _) -> 
            (val, circuit)
        Just (Nothing, gate) ->
            (val, Map.insert label (Just val, gate) newCircuit)
            where (val, newCircuit) = evalCircuit circuit (label, (Nothing, gate))
        Nothing -> error $ "Bad label " ++ label

evalSingle :: Map.Map String (Maybe Word16, Gate) -> Wire -> (Word16 -> Word16) -> (Word16, (Map.Map String (Maybe Word16, Gate)))
evalSingle circuit (Right val) fn = (fn val, circuit)
evalSingle circuit (Left label) fn = 
    (fn val, newCircuit)
    where (val, newCircuit) = lookupLabel circuit label

evalShift :: Map.Map String (Maybe Word16, Gate) -> Wire -> Int -> (Word16, (Map.Map String (Maybe Word16, Gate)))
evalShift circuit (Right val) shiftVal = (shift val shiftVal, circuit)
evalShift circuit (Left label) shiftVal =
    (shift val shiftVal, newCircuit)
    where (val, newCircuit) = lookupLabel circuit label

evalTwo :: Map.Map String (Maybe Word16, Gate) -> Wire -> Wire -> (Word16 -> Word16 -> Word16) -> (Word16, (Map.Map String (Maybe Word16, Gate)))
evalTwo circuit in1 in2 fn =
    case (in1, in2) of
        (Right val1, Right val2) -> 
            (fn val1 val2, circuit)
        (Left label1, Right val2) ->
            (fn val1 val2, newCircuit)
            where (val1, newCircuit) = lookupLabel circuit label1
        (Right val1, Left label2) ->
            (fn val1 val2, newCircuit)
            where (val2, newCircuit) = lookupLabel circuit label2
        (Left label1, Left label2) ->
            (fn val1 val2, newCircuit2)
            where (val1, newCircuit) = lookupLabel circuit label1
                  (val2, newCircuit2) = lookupLabel newCircuit label2

evalCircuit :: Map.Map String (Maybe Word16, Gate) -> (String, (Maybe Word16, Gate)) -> (Word16, (Map.Map String (Maybe Word16, Gate)))
evalCircuit circuit (label, (Just x, _)) = (x, circuit)
evalCircuit circuit (label, (Nothing, gate)) =
    case gate of
        IN in1 -> 
            (val, Map.insert label (Just val, gate) newCircuit)
            where (val, newCircuit) = evalSingle circuit in1 id
        AND in1 in2 -> 
            (val, Map.insert label (Just val, gate) newCircuit)
            where (val, newCircuit) = evalTwo circuit in1 in2 (.&.)
        OR in1 in2 -> 
            (val, Map.insert label (Just val, gate) newCircuit)
            where (val, newCircuit) = evalTwo circuit in1 in2 (.|.)
        NOT in1 ->
            (val, Map.insert label (Just val, gate) newCircuit)
            where (val, newCircuit) = evalSingle circuit in1 complement
        LSHIFT in1 shiftVal ->
            (val, Map.insert label (Just val, gate) newCircuit)
            where (val, newCircuit) = evalShift circuit in1 shiftVal
        RSHIFT in1 shiftVal ->
            (val, Map.insert label (Just val, gate) newCircuit)
            where (val, newCircuit) = evalShift circuit in1 (-shiftVal)

setKey :: Map.Map String (Maybe Word16, Gate) -> String -> Word16 -> Map.Map String (Maybe Word16, Gate) 
setKey circuit key val =
    Map.insert key (Just val, gate) circuit 
    where (_, gate) = lookupKey circuit key

lookupKey :: Map.Map String (Maybe Word16, Gate) -> String -> (Word16, Gate)
lookupKey circuit key =
    case Map.lookup key circuit of
        Just (Just val, gate) -> (val, gate)
        _ -> error $ key ++ " not in circuit or value couldn't be evaluated"

main :: IO ()
main = do 
    file <- getLine
    gatesFile <- readFile file
    let gates = map parseGate $ map words $ lines gatesFile
        circuit = Map.fromList gates
        evalStart circuit start = snd $ evalCircuit circuit start
        values = foldl evalStart circuit $ Map.toList circuit
        (newB,_) = lookupKey values "a"
        overriddenCircuit = setKey circuit "b" newB
        overriddenValues = foldl evalStart overriddenCircuit $ Map.toList overriddenCircuit
    print $ fst $ lookupKey overriddenValues "a"