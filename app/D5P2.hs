import qualified Data.Map.Lazy as M
import Data.Map.Lazy (Map)
import Data.List.Split (splitOn)

data Stacks = Stacks (Map Integer String)

getRowChars :: String -> String
getRowChars [_,x,_] = [x]
getRowChars (_:x:_:_:xs) = x : getRowChars xs
getRowChars e = error $ show e

readRow :: String -> Map Integer String
readRow =
	M.fromList .
	zip [1..] .
	map (\x -> if x == ' ' then [] else [x]) .
	getRowChars

data Instruction = Instruction
	{ amount :: Int
	, start :: Integer
	, end :: Integer
	}

readInstruction :: String -> Instruction
readInstruction i = Instruction (read a) (read s) (read e) where
	[_,a,_,s,_,e] = words i

parseInput :: String -> ([Map Integer String], [Instruction])
parseInput i = (readRow <$> init stacks, readInstruction <$> ins) where
	[stacks,ins] = splitOn [""] $ lines i

makeStartStack :: [Map Integer String] -> Stacks
makeStartStack = foldr addRow (Stacks M.empty) where
	addRow row (Stacks stack) = Stacks $ M.unionWith (<>) row stack

doInstruction :: Stacks -> Instruction -> Stacks
doInstruction (Stacks s) i =
	Stacks $
	M.adjust (newCrates <>) (end i) $
	M.adjust (drop $ amount i) (start i) $
	s
	where
		oldCrates = take (amount i) $ s M.! (start i)
		newCrates = oldCrates

doInstructions :: Stacks -> [Instruction] -> Stacks
doInstructions = foldl doInstruction

getTops :: Stacks -> String
getTops (Stacks s) = foldMap (pure . head) s

main :: IO ()
main = do
	c <- getContents
	let (rows, instructions) = parseInput c
	let startStack = makeStartStack rows
	print $ getTops $ doInstructions startStack instructions