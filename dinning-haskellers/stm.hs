{-- (c) Ryan Dickie 2007 
	Dining philosophers but they all share the same pool of forks.
	I think this is funny because i am making a lock using STM
--}
import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (finally)
import System.Random (randomRs,mkStdGen)
import Monad (zipWithM)

ungrabFork::String->TVar Int->IO()
ungrabFork name forks = do { --spinlock retry? ewww.. but it is simulated using stm which is lock free :P
			putStr (name ++ " has put back a fork\n");
			atomically ( do {numfs <- readTVar forks; writeTVar forks (numfs+1)} ) };


grabFork::String->TVar Int->IO()
grabFork name forks = do { --check will retry transaction if false
			atomically ( do {numfs <- readTVar forks; check (numfs > 0); writeTVar forks (numfs-1)} );
			putStr (name ++ " has taken a fork\n"); }

think name delayTime = do { putStr (name ++ " is Thinking\n"); threadDelay ( delayTime*100000) }
eat name delayTime   = do { putStr (name ++ " is Eating\n"); threadDelay ( delayTime*100000) }

run::String->Int->Int->TVar Int->IO()
run name thinkTime eatTime forks = do {think name thinkTime; grabFork name forks; eat name eatTime; ungrabFork name forks}

spawn names thinkTimes eatTimes forks = zipWith3M fc names thinkTimes eatTimes where
					fc = (\x y z -> forkChild (run x y z forks))

names = ["jean","marc","luc","samuel","paul","sebastien","gaetan","alain"]
randThinkTimes = (randomRs (1,21) (mkStdGen 200104032)) --wait times, 94348 is seed!
randEatTimes = (randomRs (1,21) (mkStdGen 42)) --wait times, 94348 is seed!

main::IO()
main = do {
	numforks <- atomically (newTVar 3); -- set number of forks to share
	a <- spawn names randThinkTimes randEatTimes numforks;
	waitForChildren;
	putStrLn "They are Full!";
}
-------------------------------------------------
-------------------------------------------------
-------------------------------------------------
zipWith3M f xs ys zs = sequence (zipWith3 f xs ys zs)

--list of children threads
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

--this certainly isn't a join. more like a spinlock
waitForChildren :: IO ()
waitForChildren = do
	cs <- takeMVar children
	case cs of
	  []   -> return ()
	  m:ms -> do
	    putMVar children ms
	    takeMVar m
	    waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
	 mvar <- newEmptyMVar
	 childs <- takeMVar children
	 putMVar children (mvar:childs)
	 forkIO (io `finally` putMVar mvar ())
