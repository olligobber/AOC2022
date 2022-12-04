import Data.Char (isLower, isUpper)
import Data.List (intersect)

priority :: Char -> Integer
priority c | isLower c = toInteger $ fromEnum c - 96
priority c | isUpper c = toInteger $ fromEnum c - 64 + 26

half :: [a] -> ([a],[a])
half l = (take n l, drop n l) where
	n = length l `div` 2

main :: IO ()
main = interact $
	show . sum . fmap (head . uncurry intersect . half . fmap priority) . lines