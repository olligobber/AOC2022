import Data.Char (isLower, isUpper)
import Data.List (intersect)

priority :: Char -> Integer
priority c | isLower c = toInteger $ fromEnum c - 96
priority c | isUpper c = toInteger $ fromEnum c - 64 + 26

group :: [a] -> [(a,a,a)]
group (x:y:z:xs) = (x,y,z):group xs
group [] = []

triIntersect :: Eq a => ([a], [a], [a]) -> [a]
triIntersect (x,y,z) = x `intersect` y `intersect` z

main :: IO ()
main = interact $
	show . sum . fmap (priority . head . triIntersect) . group . lines