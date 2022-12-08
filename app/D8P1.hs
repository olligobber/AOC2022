import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S

data Coordinate = Coordinate
	{ x :: Integer
	, y :: Integer
	} deriving (Eq, Ord)

type Grid = Map Coordinate Int

readGrid :: String -> Grid
readGrid i = M.fromList $ do
	(row, ty) <- zip (lines i) [1..]
	(cell, tx) <- zip row [1..]
	pure $ (Coordinate tx ty, read $ pure cell)

type Visible = Set Coordinate

getVisible :: Grid -> [Coordinate] -> Set Coordinate
getVisible = helper (0-1) where
	helper h g (c:cs)
		| g M.! c > h = S.insert c $ helper (g M.! c) g cs
		| otherwise = helper h g cs
	helper _ _ [] = mempty

getAllVisible :: Grid -> Set Coordinate
getAllVisible g = foldMap (getVisible g) lines where
	lines =
		horLines <> (reverse <$> horLines) <>
		verLines <> (reverse <$> verLines)
	minx = minimum $ x <$> M.keys g
	maxx = maximum $ x <$> M.keys g
	miny = minimum $ y <$> M.keys g
	maxy = maximum $ y <$> M.keys g
	horLines = do
		tx <- [minx .. maxx]
		pure $ do
			ty <- [miny .. maxy]
			pure $ Coordinate tx ty
	verLines = do
		ty <- [miny .. maxy]
		pure $ do
			tx <- [minx .. maxx]
			pure $ Coordinate tx ty

main :: IO ()
main = interact $ show . length . getAllVisible . readGrid