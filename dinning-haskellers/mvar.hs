{--
 - (c) Ryan Dickie 2007
 - someone on IRC inspired me to solve this problem in haskell using STM 
 - i don't have the STM compiled yet so i wrote it with the old style MVar
 - also.. there is only n forks placed in the center of the table for all
 - so this is more of a "locking" excersize than anything
--}
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (finally)
import System.Random (randomRs,mkStdGen)
import Monad (zipWithM)

--in a weird way i'm using it like a semaphore
numberOfForks = 2
fork = unsafePerformIO (newMVar numberOfForks)

ungrabFork name = do 
		putStrLn (name ++ " has returned the fork")
		numfs <- tryTakeMVar fork --this makes it unatomic!!! soln: add another mutex!
		incvar numfs where
			incvar Nothing = putMVar fork 1
			incvar (Just x) = putMVar fork (x+1)

grabFork name = do
		numfs <- takeMVar fork --effectively locks! 
		decvar numfs where
			decvar 1 = putStrLn (name ++ " has grabbed the last fork")
			decvar x = do { putMVar fork (x-1); putStrLn (name ++ " has grabbed a fork") }

thinkPhilosopher name delayTime = do { putStrLn (name ++ " is Thinking"); threadDelay ( delayTime*100000) }
eatPhilosopher name delayTime   = do { putStrLn (name ++ " is Eating"); threadDelay ( delayTime*100000) }
runPhilosopher name thinkTime eatTime = do { thinkPhilosopher name thinkTime;
					grabFork name;
					eatPhilosopher name eatTime;
					ungrabFork name }

spawnPhils names thinkTimes eatTimes = zipWith3M (\x y z -> forkChild (runPhilosopher x y z)) names thinkTimes eatTimes
zipWith3M f xs ys zs = sequence (zipWith3 f xs ys zs)

names = ["jean","marc","luc","samuel","paul"]
randThinkTimes = (randomRs (1,21) (mkStdGen 200104032)) --wait times, 94348 is seed!
randEatTimes = (randomRs (1,21) (mkStdGen 42)) --wait times, 94348 is seed!

main::IO()
main = do {
	spawnPhils names randThinkTimes randEatTimes;
	waitForChildren;
	putStrLn "They are Full!";
}

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
