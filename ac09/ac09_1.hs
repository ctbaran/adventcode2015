import Debug.Trace
import qualified Data.Map as Map

type Edge = (String, Integer)

addEdge :: String -> Integer -> Maybe [Edge] -> Maybe [Edge]
addEdge to distance Nothing = Just ([(to, distance)])
addEdge to distance (Just edges) = Just (edges ++ [(to, distance)])

addPair :: Map.Map String [Edge] -> (String, String, Integer) -> Map.Map String [Edge]
addPair cities (from, to, distance) =
    Map.alter (addEdge from distance) to citiesFirst 
    where citiesFirst = Map.alter (addEdge to distance) from cities

parsePath :: [String] -> (String, String, Integer)
parsePath (from:_:to:_:distance:_) = (from, to, read distance :: Integer)

getEdges :: String -> Map.Map String [Edge] -> [Edge]
getEdges city cities =
    case Map.lookup city cities of
        Just edges ->
            filter isMember edges
        Nothing -> []
    where isMember edge = Map.member (fst edge) cities

findPaths :: Map.Map String [Edge] -> Integer -> String -> [Integer]
findPaths cities dist city
    | (Map.size cities) == 1 = [dist]
    | otherwise =
        foldl visitEdge [] edges
        where edges = getEdges city cities
              visitCity = Map.delete city cities
              visitEdge paths (edgeCity, edgeDist) = paths ++ findPaths visitCity (dist+edgeDist) edgeCity

main :: IO ()
main = do 
    file <- getLine
    pathsFile <- readFile file
    let paths = map (parsePath . words) $ lines pathsFile
        cities = foldl addPair Map.empty paths
        keys = map fst $ Map.toList cities
        completePaths = map (findPaths cities 0) keys
        shortestPath = minimum $ foldl (++) [] completePaths
    print $ shortestPath