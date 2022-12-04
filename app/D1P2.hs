import Data.List.Split (splitOn)
import Data.List (delete)

splitInputs :: String -> [[String]]
splitInputs x = words <$> splitOn "\n\n" x

totalElf :: [String] -> Integer
totalElf x = sum $ read <$> x

best :: Integer -> [Integer] -> [Integer]
best 0 _ = []
best n l = maximum l : best (n-1) (delete (maximum l) l)

main :: IO ()
main = interact $ show . sum . best 3 . map totalElf . splitInputs