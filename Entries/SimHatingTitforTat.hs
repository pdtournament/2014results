module Entries.SimHatingTitForTat where

import Tournament
import Bots

simHatingTitForTat :: Bot
simHatingTitForTat = Bot run
  where run op hist
            | null hist    = return Cooperate
            | allGood hist = return . snd $ last hist
            | otherwise    = return Defect
        allGood l = tail (map fst l) == init (map snd l)

{-
simCheckerChecker :: Bot
simCheckerChecker = Bot (\op hist -> if null hist then return Cooperate else do
    -- simulate previous round
    sim <- time 10000 . runBot op simCheckerChecker . invert $ init hist
    if sim == snd (last hist)
        then return Cooperate
        else return Defect)
-}
