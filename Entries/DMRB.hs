-- My game plan is: against bots who try to simulate me, don't try to be clever,
-- just play tit-for-tat. Against bots who don't try to simulate me, I can
-- predict their moves, so exploit them.

--   Try not to take too long when simulating: bots who try to simulate me but
-- time out, might decide to defect against me on principle. I think it would be
-- silly to do this with a time limit as low as 10000 μs, as recommended in the
-- explanation, especially if `time` has an overhead close to that amount (which
-- I don't observe myself, but it's not my computer that it'll be running
-- on...). I give myself a limit of 18000 μs, and if that times out I go for the
-- tit-for-tat option. I should always return within 20000 μs.

--   Naturally, how well I do depends on the other bots in play. JusticeBot
-- fucks me over, for example: we fall into (D,D) when other bots get (C,C)
-- against ver (potentially even (D,C), but I'm not sure what that bot would be
-- doing). I'm hoping JusticeBot gets eliminated early on, because as it stands,
-- I'm not even winning the tournament with the toy bots.

--   I also frequently cooperate against randomBot, which is bad. I expect
-- randomBot and anything like it to go quickly, and it's only an expected
-- difference of 1.5pts/round when I do so, but still. If I spent more time
-- simulating, I would cooperate less.

module Entries.DMRB where

import Bots
import Tournament

import Control.Monad (replicateM)

-- Only used in `timeIt`, which is only used for testing.
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

davidMonRoBot :: Bot
davidMonRoBot = dmrbGen 7 3

-- 18000 means that I should at least always return a result to people who give
-- me 20000.
dmrbGen :: Int -> Int -> Bot
dmrbGen iterations rounds = Bot run where
  run op hist = do
    move <- time 18000 $ multiExploit iterations rounds op hist
    return $ case move of
      (Just x) -> x
      (Nothing) -> case hist of
        [] -> Cooperate
        xs -> snd $ last xs

-- Run `exploit` some number of times, going some number of rounds deep, and
-- return the move that has the best average results.
--   (Currently doesn't do the average.)
multiExploit :: BotEnvironment m => Int -> Int -> Bot -> [Moves] -> m Choice
multiExploit iterations rounds bot hist = do
  itsMoves <- replicateM iterations $ dumbBotsNextMove bot hist
  movesAndScores <- mapM doExploit itsMoves
  let (co, de) = avgScores movesAndScores
   in return $ if co > de then Cooperate else Defect
  where doExploit = exploit (min rounds (99 - (length hist))) bot hist

-- Int - number of rounds forward to predict
-- Bot - opponent to try to exploit.
-- [Moves] - history to start with.
-- Choice - the move it's going to do next round.
-- m (Choice, Int) - my best move, and how many points I expect from it.
exploit :: BotEnvironment m =>
    Int -> Bot -> [Moves] -> Choice -> m (Choice, Int)

exploit 0 bot hist itsMove
  = return (Defect, fst $ totalScores $ hist ++ [(Defect, itsMove)])

-- Work out what it's going to do next round for each of my possible moves this
-- round, and recurse to find out which one is better for me.
exploit rounds bot hist itsMove
  = do
    itsNextMoveCo <- runBot bot cooperateBot $ invert $ withMove Cooperate
    itsNextMoveDe <- runBot bot cooperateBot $ invert $ withMove Defect

    (_, scoreCo) <- recurse Cooperate itsNextMoveCo
    (_, scoreDe) <- recurse Defect itsNextMoveDe

    if scoreCo > scoreDe
      then return (Cooperate, scoreCo)
      else return (Defect, scoreDe)

  where withMove m = hist ++ [(m, itsMove)]
        recurse me you = exploit (rounds - 1) bot (withMove me) you


avgScores :: (Integral a, Fractional b) => [(Choice, a)] -> (b, b)
avgScores xs = let ((c,d), (nc,nd)) = agg' (0, 0) (0, 0) xs
                in ((fromIntegral c)/(fromIntegral $ max nc 1),
                    (fromIntegral d)/(fromIntegral $ max nd 1))
  where agg' (c,d) (nc,nd) [] = ((c,d), (nc,nd))
        agg' (c,d) (nc,nd) ((Cooperate, s) : xs) = agg' (c+s, d) (nc+1, nd) xs
        agg' (c,d) (nc,nd) ((Defect, s) : xs) = agg' (c, d+s) (nc, nd+1) xs

-- If my opponent is dumb, find their next move. If my opponent is not dumb, run
-- forever.
dumbBotsNextMove :: BotEnvironment m => Bot -> [Moves] -> m Choice
dumbBotsNextMove bot hist = runBot bot timeoutBot hist

-- Run forever. This is not the bot you're looking for.
timeoutBot :: Bot
timeoutBot = Bot run where
  run op hist = do
    infiniteLoop
    return Cooperate
  infiniteLoop = infiniteLoop

-- Wrap an 'IO' computation so that it prints execution time in
-- microseconds. System.TimeIt provides this, but uses seconds. This isn't used
-- in my bot at all, I just want it for testing.
timeIt :: IO a -> IO a
timeIt ioa = do
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    printf "CPU time: %d\n" $ (t2-t1) `div` (10^6)
    return a
