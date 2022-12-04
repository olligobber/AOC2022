data RPS = Rock | Paper | Scissors deriving (Eq)

data Outcome = Win | Lose | Draw deriving (Eq)

decryptTheir :: String -> RPS
decryptTheir "A" = Rock
decryptTheir "B" = Paper
decryptTheir "C" = Scissors
decryptTheir x = error $ x

decryptYour :: String -> Outcome
decryptYour "X" = Lose
decryptYour "Y" = Draw
decryptYour "Z" = Win

scoreOutcome :: RPS -> Outcome -> Integer
scoreOutcome _ Win = 6
scoreOutcome _ Draw = 3
scoreOutcome _ Lose = 0

getChoice :: RPS -> Outcome -> RPS
getChoice x Draw = x
getChoice Rock Win = Paper
getChoice Paper Win = Scissors
getChoice Scissors Win = Rock
getChoice x Lose = getChoice (getChoice x Win) Win

scoreChoice :: RPS -> Integer
scoreChoice Rock = 1
scoreChoice Paper = 2
scoreChoice Scissors = 3

scoreRound :: RPS -> Outcome -> Integer
scoreRound x y = scoreChoice (getChoice x y) + scoreOutcome x y

processLine :: String -> Integer
processLine s = scoreRound (decryptTheir x) (decryptYour y) where
	[x,y] = words s

main :: IO ()
main = interact $ show . sum . map processLine . lines