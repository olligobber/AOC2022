import Data.List (tails)

isValidTail :: Eq a => [a] -> Bool
isValidTail i = case take 4 i of
	[a,b,c,d] -> a /= b && a /= c && a /= d && b /= c && b /= d && c /= d
	_ -> False

getFirstMarker :: Eq a => [a] -> Integer
getFirstMarker =
	fst . head . dropWhile (not . isValidTail . snd) . zip [4..] . tails

main :: IO ()
main = interact $ show . getFirstMarker