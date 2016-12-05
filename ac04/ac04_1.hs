import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List
import Crypto.Hash

md5 :: LB.ByteString -> Digest MD5
md5 = hashlazy

find_hash :: LB.ByteString -> (Digest MD5, LB.ByteString)
find_hash str = 
	let 
		strings = [LB.append str (LB.pack $ show x) | x <- [1..] ]
		goodhash (hash, _) = "00000" == take 5 (show hash)
	in
	head $ filter goodhash $ map (\x->(md5 x, x)) strings

main = do 
    key <- LB.readFile "input.txt"
    print $ (find_hash key)
