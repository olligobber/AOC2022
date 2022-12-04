import Data.List.Split (splitOn)

splitInputs :: String -> [[String]]
splitInputs x = words <$> splitOn "\n\n" x

totalElf :: [String] -> Integer
totalElf x = sum $ read <$> x

main :: IO ()
main = interact $ show . maximum . map totalElf . splitInputs