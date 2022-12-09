import Prelude hiding (Either(..))
import Data.Set (Set)
import qualified Data.Set as Se
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as St
import Data.Foldable (fold)

data Position = Position
	{ x :: Integer
	, y :: Integer
	} deriving (Eq, Ord)

startPos :: Position
startPos = Position 0 0

data Direction = Up | Right | Left | Down deriving (Eq)

readDir :: String -> Direction
readDir "R" = Right
readDir "L" = Left
readDir "U" = Up
readDir "D" = Down

readMovement :: String -> [Direction]
readMovement i = replicate (read n) (readDir d) where
	[d, n] = words i

readMovements :: String -> [Direction]
readMovements i = lines i >>= readMovement

moveHead :: Direction -> Position -> Position
moveHead dir pos = case dir of
	Up -> Position (x pos) (y pos + 1)
	Down -> Position (x pos) (y pos - 1)
	Left -> Position (x pos + 1) (y pos)
	Right -> Position (x pos - 1) (y pos)

-- given head and then tail position, move
moveTail :: Position -> Position -> Position
moveTail headPos tailPos
	| d x `elem` [-1 .. 1] && d y `elem` [-1..1] = tailPos
	| otherwise = Position (x tailPos + signum (d x)) (y tailPos + signum (d y))
	where
		d z = z headPos - z tailPos

data MoveState = MoveState
	{ headP :: Position
	, tailP :: Position
	} deriving (Eq, Ord)

type Log = Set Position

moveState :: Direction -> State MoveState Log
moveState dir = do
	oldHead <- St.gets headP
	oldTail <- St.gets tailP
	let newHead = moveHead dir oldHead
	let newTail = moveTail newHead oldTail
	St.put $ MoveState newHead newTail
	pure $ Se.singleton newTail

startState :: MoveState
startState = MoveState startPos startPos

allMoves :: [Direction] -> Log
allMoves dirs = fold $ St.evalState (traverse moveState dirs) startState

main :: IO ()
main = interact $ show . length . allMoves . readMovements