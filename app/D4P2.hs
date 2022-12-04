import Data.List.Split (splitOn)

data Range = Range
	{ start :: Integer
	, end :: Integer
	} deriving (Eq, Show)

inRange :: Integer -> Range -> Bool
inRange x r = start r <= x && x <= end r

overlaps :: Range -> Range -> Bool
overlaps a b = inRange (start a) b || inRange (end a) b || inRange (start b) a || inRange (end b) a

parseRange :: String -> Range
parseRange i = Range (read s) (read e) where
	[s,e] = splitOn "-" i

parsePair :: String -> (Range, Range)
parsePair i = (parseRange a, parseRange b) where
	[a,b] = splitOn "," i

main :: IO ()
main = interact $ show . length . filter (uncurry overlaps) . fmap parsePair . lines