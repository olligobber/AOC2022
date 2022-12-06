import Data.List (tails, sort, nub)

isValidTail :: Ord a => [a] -> Bool
isValidTail i = length (nub $ sort $ take 14 i) == 14

getFirstMarker :: Ord a => [a] -> Integer
getFirstMarker =
	fst . head . dropWhile (not . isValidTail . snd) . zip [14..] . tails

main :: IO ()
main = interact $ show . getFirstMarker