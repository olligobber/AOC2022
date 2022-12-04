import Data.List.Split (splitOn)

data Range = Range
	{ start :: Integer
	, end :: Integer
	} deriving Eq

contains :: Range -> Range -> Bool
contains big small = start big <= start small && end small <= end big

reconsider :: Range -> Range -> Bool
reconsider a b = contains a b || contains b a

parseRange :: String -> Range
parseRange i = Range (read s) (read e) where
	[s,e] = splitOn "-" i

parsePair :: String -> (Range, Range)
parsePair i = (parseRange a, parseRange b) where
	[a,b] = splitOn "," i

count :: [Bool] -> Integer
count = sum . fmap (\x -> if x then 1 else 0)

main :: IO ()
main = interact $ show . count . fmap (uncurry reconsider . parsePair) . lines