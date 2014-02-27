--(c)2007 Ryan Dickie todo: comment this code!!!
--version 5: i made a few minor tweaks and the code runs 2-3x faster
import Data.Array.Diff
import Data.List (foldl1')
import System.Random

data Dir = North | West | East | South deriving (Enum)

isMember pos paths = gp (paths!pos) where
	gp ' ' = True
	gp _ = False

isValidDir paths (x,y) dir = (notBorder dir) && (not (isMember (movePoint (x,y) dir) paths)) && (not (isBreakingWall paths (movePoint (x,y) dir) dir))
	where   notBorder North = y > 1 
		notBorder West = x > 1
		notBorder East  = x < width - 2
		notBorder South = y < height - 2

isBreakingWall paths pos North = ( (movePoint pos North) `isMember` paths ) || ((movePoint pos East) `isMember` paths) || ((movePoint pos West) `isMember` paths)
isBreakingWall paths pos West = ( (movePoint pos North) `isMember` paths ) || ((movePoint pos South) `isMember` paths) || ((movePoint pos West) `isMember` paths)
isBreakingWall paths pos East = ( (movePoint pos North) `isMember` paths ) || ((movePoint pos East) `isMember` paths) || ((movePoint pos South) `isMember` paths)
isBreakingWall paths pos South = ( (movePoint pos South) `isMember` paths ) || ((movePoint pos East) `isMember` paths) || ((movePoint pos West) `isMember` paths)

cornered paths pos = not $ foldl1' (||) $ map (isValidDir paths pos) [North,West,East,South]

getMark paths point = visited (point `isMember` paths)
	where   visited True = ' '
		visited False = '@'

genPath paths [] _ = paths --base case where stack is empty!
genPath paths stack dirList =  --recursive step
	if (isValidDir paths pos dir)  then
		genPath (paths//[(nextPoint,' ')]) ( nextPoint : stack ) nextDirList
            	else if (cornered paths pos) then
			genPath paths (tail stack) dirList
		else
			genPath paths stack (drop (forwardness-1) nextDirList)
	where 	pos = head stack
		dir = head dirList
		nextDirList = tail dirList
		nextPoint = movePoint pos dir

movePoint (x,y) North = (x,y-1)
movePoint (x,y) West = (x-1,y)
movePoint (x,y) East = (x+1,y)
movePoint (x,y) South = (x,y+1)

--showMaze paths = sequence_ (map printRow [0..height-1]) where 
showMaze paths = mapM_ printRow [0..height-1] where 
	printRow row = putStrLn $ [ getMark paths (x,row) | x <- [0..width-1] ]

width = 1000
height = 1000
forwardness = 4 --try and head forward at least this many times before branching

main :: IO ()
main = do
	x <- randomRIO (1::Int,width-2)
	y <- randomRIO (1::Int,height-2)
	let dL = concatMap (replicate forwardness) $ (randomRs (0,3) (mkStdGen 42))
	let dirList = map toEnum dL --my old method was ubber slow. this is magically fast
	let arr = array ((0,0),(width-1,height-1)) [((x,y),'@')|x<-[0..width-1], y<-[0..height-1]] :: DiffUArray (Int,Int) Char
	showMaze $ genPath arr [(x,y)] dirList
