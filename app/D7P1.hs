import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as S
import Data.Foldable (toList)

data Directory = Directory
	{ subdirs :: Map String Directory
	, files :: Map String Integer
	}

newDir :: Directory
newDir = Directory M.empty M.empty

addFile :: String -> Integer -> Directory -> Directory
addFile name fsize dir =
	Directory
	(subdirs dir)
	(M.insert name fsize $ files dir)

-- Doesn't add if it already exists
addDir :: String -> Directory -> Directory
addDir name dir =
	Directory
	(M.insertWith (const id) name newDir $ subdirs dir)
	(files dir)

-- executes a function in a given subfolder
inDir :: [String] -> (Directory -> Directory) -> Directory -> Directory
inDir [] f dir = f dir
inDir (d:ds) f dir =
	Directory
	(M.adjust (inDir ds f) d $ subdirs dir)
	(files dir)

-- Get the sizes of this directory and its subdirectories
getDirSizes :: Directory -> (Integer, [Integer])
getDirSizes dir = (thisSize, subDirSizes <> descendentSizes) where
	thisSize = foldl (+) 0 $ subDirSizes <> fileSizes
	fileSizes = toList $ files dir
	subDirSizes = foldMap (pure . fst . getDirSizes) $ subdirs dir
	descendentSizes = foldMap (snd . getDirSizes) $ subdirs dir

data FileSys = FileSys
	{ currentDir :: [String]
	, root :: Directory
	}

modifyDir :: ([String] -> [String]) -> FileSys -> FileSys
modifyDir f sys =
	FileSys
	(f $ currentDir sys)
	(root sys)

modifyRoot :: (Directory -> Directory) -> FileSys -> FileSys
modifyRoot f sys =
	FileSys
	(currentDir sys)
	(f $ root sys)

modifyCurrent :: (Directory -> Directory) -> FileSys -> FileSys
modifyCurrent f sys =
	FileSys
	(currentDir sys)
	(inDir (currentDir sys) f $ root sys)

blank :: FileSys
blank = FileSys [] newDir

processCommand :: String -> State FileSys ()
processCommand "$ cd /" = S.modify $ modifyDir $ const []
processCommand "$ cd .." = S.modify $ modifyDir init
processCommand "$ ls" = pure ()
processCommand i = case words i of
	["$","cd",d] -> S.modify $ modifyDir $ (<> [d])
	["dir",d] -> S.modify $ modifyCurrent $ addDir d
	[s,n] -> S.modify $ modifyCurrent $ addFile n (read s)

getResult :: Directory -> Integer
getResult = sum . filter (<= 100000) . snd . getDirSizes

main :: IO ()
main = interact $
	show .
	getResult .
	root .
	flip S.execState blank .
	mapM_ processCommand .
	lines