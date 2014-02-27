import System.Random
import Array
import Monad

data Dir = North | West | East | South deriving (Eq, Enum)
type Point = (Int,Int)

isValidDir paths pos dir = (notBorder dir) && (not ((movePoint pos dir) `elem` paths)) && (not (isBreakingWall paths (movePoint pos dir) dir))
	where   notBorder North = (snd pos) > 1 
		notBorder West = (fst pos) > 1
		notBorder East  = (fst pos) < width - 2
		notBorder South = (snd pos) < height - 2

isBreakingWall paths pos North = ( (movePoint pos North) `elem` paths ) || ((movePoint pos East) `elem` paths) || ((movePoint pos West) `elem` paths)
isBreakingWall paths pos West = ( (movePoint pos North) `elem` paths ) || ((movePoint pos South) `elem` paths) || ((movePoint pos West) `elem` paths)
isBreakingWall paths pos East = ( (movePoint pos North) `elem` paths ) || ((movePoint pos East) `elem` paths) || ((movePoint pos South) `elem` paths)
isBreakingWall paths pos South = ( (movePoint pos South) `elem` paths ) || ((movePoint pos East) `elem` paths) || ((movePoint pos West) `elem` paths)

cornered paths pos = not $ foldl1 (||) $ map (isValidDir paths pos) [North,West,East,South]

getMark paths point = visited (point `elem` paths)
	where   visited True = ' '
		visited False = '@'

genMaze w h paths = array ((0,0),(w-1,h-1)) [ ( (x,y) , getMark paths (x,y) ) | x <- [0..w-1], y<-[0..h-1] ]

genPath paths [] _ = paths --base case where stack is empty!
genPath paths stack dirList =  --recursive step
	if (isValidDir paths pos dir)  then
		genPath ( nextPoint : paths) ( nextPoint : stack ) nextDirList
            	else if (cornered paths pos) then
			genPath paths (tail stack) dirList
		else
			genPath paths stack nextDirList
	where 	pos = head stack
		dir = head dirList
		nextDirList = tail dirList
		nextPoint = movePoint pos dir

movePoint pos North = (fst pos, (snd pos) - 1)
movePoint pos West  = ((fst pos)-1, snd pos)
movePoint pos East  = ((fst pos)+1, snd pos)
movePoint pos South = (fst pos, (snd pos) + 1)

showMaze maze = sequence_ (map printRow [0..height-1]) where 
	printRow row = putStrLn $  concat $ [ [(maze!(x,row)),','] | x <- [0..width-1] ]

width = 400
height = 200
forwardness = 4 --try and head forward at least this many times before branching

main :: IO ()
main = do 
	x <- randomRIO (1::Int,width-2)
	y <- randomRIO (1::Int,height-2)
	let dL = concat $ map (replicate forwardness) $ (randomRs (0,3) (mkStdGen 42))
	let dirList = map ([North,South,East,West]!!) dL
	let p = genPath [(x,y)] [(x,y)] dirList
	let w = genMaze width height p
	showMaze w
