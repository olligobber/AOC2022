data RPS = Rock | Paper | Scissors deriving (Eq)

decryptTheir :: String -> RPS
decryptTheir "A" = Rock
decryptTheir "B" = Paper
decryptTheir "C" = Scissors
decryptTheir x = error $ x

decryptYour :: String -> RPS
decryptYour "X" = Rock
decryptYour "Y" = Paper
decryptYour "Z" = Scissors

scoreOutcome :: RPS -> RPS -> Integer
scoreOutcome x y | x == y = 3
scoreOutcome Rock Paper = 6
scoreOutcome Paper Scissors = 6
scoreOutcome Scissors Rock = 6
scoreOutcome _ _ = 0

scoreChoice :: RPS -> RPS -> Integer
scoreChoice _ Rock = 1
scoreChoice _ Paper = 2
scoreChoice _ Scissors = 3

scoreRound :: RPS -> RPS -> Integer
scoreRound x y = scoreChoice x y + scoreOutcome x y

processLine :: String -> Integer
processLine s = scoreRound (decryptTheir x) (decryptYour y) where
	[x,y] = words s

main :: IO ()
main = interact $ show . sum . map processLine . lines