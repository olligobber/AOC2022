import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

data Coordinate = Coordinate
	{ x :: Integer
	, y :: Integer
	} deriving (Eq, Ord, Show)

type Grid = Map Coordinate Int

readGrid :: String -> Grid
readGrid i = M.fromList $ do
	(row, ty) <- zip (lines i) [1..]
	(cell, tx) <- zip row [1..]
	pure $ (Coordinate tx ty, read $ pure cell)

getVisible :: Grid -> Int -> [Coordinate] -> Integer
getVisible g e (c:cs)
	| g M.! c >= e = 1
	| otherwise = 1 + getVisible g e cs
getVisible _ _ [] = 0

getScore :: Grid -> Coordinate -> Integer
getScore g c =
	product $ getVisible g (g M.! c) <$> sightlines
	where
		sightlines = [upline, downline, leftline, rightline]
		minx = minimum $ x <$> M.keys g
		maxx = maximum $ x <$> M.keys g
		miny = minimum $ y <$> M.keys g
		maxy = maximum $ y <$> M.keys g
		upline = Coordinate (x c) <$> [y c - 1, y c - 2 .. miny]
		downline = Coordinate (x c) <$> [y c + 1 .. maxy]
		leftline = flip Coordinate (y c) <$> [x c - 1, x c - 2 .. minx]
		rightline = flip Coordinate (y c) <$> [x c + 1 .. maxx]

bestScore :: Grid -> Integer
bestScore g = maximum $ getScore g <$> M.keys g

main :: IO ()
main = interact $ show . bestScore . readGrid
