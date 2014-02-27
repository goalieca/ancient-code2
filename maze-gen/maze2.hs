--(c)2007 Ryan Dickie todo: comment this code!!!
import Data.Set (member,insert,empty)
import Data.List (foldl1')
import System.Random
import Monad

data Dir = North | West | East | South deriving (Eq, Enum)

isValidDir paths pos dir = (notBorder dir) && (not (member (movePoint pos dir) paths)) && (not (isBreakingWall paths (movePoint pos dir) dir))
	where   notBorder North = (snd pos) > 1 
		notBorder West = (fst pos) > 1
		notBorder East  = (fst pos) < width - 2
		notBorder South = (snd pos) < height - 2

isBreakingWall paths pos North = ( (movePoint pos North) `member` paths ) || ((movePoint pos East) `member` paths) || ((movePoint pos West) `member` paths)
isBreakingWall paths pos West = ( (movePoint pos North) `member` paths ) || ((movePoint pos South) `member` paths) || ((movePoint pos West) `member` paths)
isBreakingWall paths pos East = ( (movePoint pos North) `member` paths ) || ((movePoint pos East) `member` paths) || ((movePoint pos South) `member` paths)
isBreakingWall paths pos South = ( (movePoint pos South) `member` paths ) || ((movePoint pos East) `member` paths) || ((movePoint pos West) `member` paths)

cornered paths pos = not $ foldl1' (||) $ map (isValidDir paths pos) [North,West,East,South]

getMark paths point = visited (point `member` paths)
	where   visited True = ' '
		visited False = '@'

genPath paths [] _ = paths --base case where stack is empty!
genPath paths stack dirList =  --recursive step
	if (isValidDir paths pos dir)  then
		genPath (insert nextPoint paths) ( nextPoint : stack ) nextDirList
            	else if (cornered paths pos) then
			genPath paths (tail stack) dirList
		else
			genPath paths stack (drop (forwardness-1) nextDirList)
	where 	pos = head stack
		dir = head dirList
		nextDirList = tail dirList
		nextPoint = movePoint pos dir

movePoint pos North = (fst pos, (snd pos) - 1)
movePoint pos West  = ((fst pos)-1, snd pos)
movePoint pos East  = ((fst pos)+1, snd pos)
movePoint pos South = (fst pos, (snd pos) + 1)

showMaze paths = sequence_ (map printRow [0..height-1]) where 
	printRow row = putStrLn $  concat $ [ [getMark paths (x,row), p x row] | x <- [0..width-1] ]
	p 39 19 = '\0' --to appease CGamesPlay
	p _ _ = ','

width = 1000
height = 1000
forwardness = 4 --try and head forward at least this many times before branching

main :: IO ()
main = do 
	x <- randomRIO (1::Int,width-2)
	y <- randomRIO (1::Int,height-2)
	let dL = concat $ map (replicate forwardness) $ (randomRs (0,3) (mkStdGen 42))
	let dirList = map ([North,South,East,West]!!) dL
	showMaze $ genPath (insert (x,y) empty) [(x,y)] dirList
