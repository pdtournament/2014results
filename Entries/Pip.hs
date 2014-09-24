-- Copyright (c) 2014, Alfonso De Gregorio.
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Alfonso De Gregorio nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module Entries.Pip (
    pipBot
    ) where

import           Control.Monad (replicateM)
import           Data.Maybe    (fromMaybe)
import           Prelude

import           Bots
import           Tournament

data Behavior = UnconditionalCooperator
              | UnconditionalDefector
              | DefectOnLastInTheFaceOfCooperation
              | DefectOnLastInTheFaceOfDefection
              | CooperateOnLastInTheFaceOfCooperation
              | CooperateOnLastInTheFaceOfDefection
              | Uncompromising
              | Nondeterministic
              | DefectOnTimeout
              | CooperateOnTimeout
              | Timeout
              | Retaliating
              | Forgiving
              | Simulate
              | Extortionist
--              | Clear
--              | Nice
--              | Provocable
--              | NonEnvious
              | Nil
    deriving (Show, Read, Eq, Bounded, Enum)


-- Always cooperates
helperCooperateBot :: Bot
helperCooperateBot = Bot (\_ _ -> return Cooperate)

-- "You never get used to the feeling of hot metal, entering your skull
-- and exiting through the back of your head. It's simulated in glorious
-- detail."
--                                --  Hannu Rajaniemi, The Quantum Thief
-- Always defects
helperDefectBot :: Bot
helperDefectBot = Bot (\_ _ -> return Defect)

-- 50% probability of cooperating, 50% probability of defecting
helperRandomBot :: Bot
helperRandomBot = Bot (\_ _ -> do
    choice <- rand
    return (if choice < 0.5 then Cooperate else Defect))

-- "'It Got you again', he says, not bothering to look up. 'Again. What is
-- that, three times in a row now? You should know by now that it always
-- goes for tit-for-tat.'"
--                                   -- Hannu Rajaniemi, The Quantum Thief
-- Do whatever my opponent did in the previous round; if it is the first round,
-- cooperate.
helperTitForTatBot :: Bot
helperTitForTatBot = Bot (\_ history -> case history of
            [] -> return Cooperate
            xs -> return . snd $ last xs)

-- "Suddenly, it feels amazing that I haven't thought about it for so long.
-- I have been too preoccupied with guns, defection and cooperation."
--                                   --  Hannu Rajaniemi, The Quantum Thief
--
-- Just waste some time
helperTimeWasterBot :: Bot
helperTimeWasterBot = Bot (\op hist -> do
    _ <- runBot op op $ invert hist
    return Defect)

-- What if simulation: How the opponent would behave given multiple alternative
-- histories of past interactions?
simulate :: BotEnvironment m => Int -> Int -> Bot -> Bot -> [[Moves]] -> m [[Maybe Choice]]
simulate runs timeout bot opp histories = mapM (replicateM runs . time timeout . runBot bot opp) histories

-- Compute probabilities of cooperation
cprob :: Fractional a => [[Maybe Choice]] -> [a]
cprob simulations = ps
    where ns = map (length) (map (filter ((== Just Cooperate))) simulations)
          noc = map (length) (map (filter ((== Nothing))) simulations)
          runs = map (length) simulations
          ps = zipWith (/) (map (fromIntegral) ns) (map (fromIntegral) (zipWith (-) runs noc))

-- Are all the elements in the list the same?
allthesame :: (Eq a) => [a] -> Bool
allthesame xs = all (== head xs) (tail xs)

-- Who are we dealing with?
basicProfiler :: BotEnvironment m => Bot -> Int -> m [Maybe Behavior]
basicProfiler op r = do
             -- Let's forget past interactions for a second...
             -- Simulate the opponent default behavior, given no previous
             -- interaction. Has the opponent the ability to defect?
             -- Does it cooperates?
             simsd <- simulate 5 10000 op helperDefectBot [[]]
             simsc <- simulate 5 10000 op helperCooperateBot [[]]

             -- "'You have a lot to learn about being a criminal. It's all
             -- about the waiting. Boredom punctuated by flashes of sheer
             -- terror. Sort of like war."
             --                      -- Hannu Rajaniemi, The Quantum Thief
             --
             -- Does it defect on last turn in the face of cooperation
             -- and/or defection?
             simsld <- simulate 3 10000 op helperDefectBot [replicate (r - 1) (Cooperate,Cooperate)]
             simslc <- simulate 3 10000 op helperCooperateBot [replicate (r - 1) (Cooperate,Cooperate)]

             simsr <- simulate 1 10000 op helperCooperateBot [invert [(Defect,Cooperate)]]
             simsf <- simulate 1 10000 op helperCooperateBot [invert [(Defect,Cooperate),(Cooperate,Defect)]]

             -- What if we provide some synthetic history and defect?
             simsdd <- simulate 19 10000 op helperTitForTatBot [[(Cooperate,Defect)]]
             -- simsdc <- simulate 7 10000 op helperCooperateBot [replicate (r - 2) (Cooperate,Defect)]

-- Extortionist
--             simsec <- (if   (    (head $ cprob simsd) > 0.0
--                               && (head $ cprob simsd) < 1.0 )
--                          || (    (head $ cprob simsdd) > 0.0
--                               && (head $ cprob simsdd) < 1.0)
--                        then simulate 5 20000 op helperTitForTatBot [[(Cooperate,Cooperate)]]
--                        else simulate 1 1 helperDefectBot helperDefectBot [[]])
--             simsed <- (if   (    (head $ cprob simsd) > 0.0
--                               && (head $ cprob simsd) < 1.0 )
--                          || (    (head $ cprob simsdd) > 0.0
--                               && (head $ cprob simsdd) < 1.0)
--                        then simulate 3 20000 op helperTitForTatBot [[(Defect,Defect)]]
--                        else simulate 1 1 helperCooperateBot helperCooperateBot [[]])

             -- How the opponent strategy handle non-deterministic behaviors?
             simsuc <- simulate 7 10000 op helperRandomBot [[]]
             simsto <- simulate 3 20000 op helperTimeWasterBot [[]]

             -- 'Oh, war was much better,' she says, excitedly. 'We were in
             -- the Protocol War. I loved it. You get to think so fast."
             --                       -- Hannu Rajaniemi, The Quantum Theif
             simssi <- simulate 3 200 op helperTimeWasterBot [[]]

             let coopDefector = (Just Defect `elem` (head $ simsd) || Nothing `elem` (head $ simsd) )
             let coopCooperator = not (Just Defect `elem` (head $ simsc) || Nothing `elem` (head $ simsc) )

             let defDefector = not (Just Cooperate `elem` (head $ simsd) || Nothing `elem` (head $ simsd) )
             let defCooperator = (Just Cooperate `elem` (head $ simsc) || Nothing `elem` (head $ simsc) )

             return [
                      (if (not coopDefector && coopCooperator)
                       then Just UnconditionalCooperator
                       else Nothing)
                    , (if (not defCooperator && defDefector)
                       then Just UnconditionalDefector
                       else Nothing)
                    , (if (head $ cprob simslc) == 0.0
                       then Just DefectOnLastInTheFaceOfCooperation
                       else Nothing)
                    , (if (head $ cprob simsld) == 0.0
                       then Just DefectOnLastInTheFaceOfDefection
                       else Nothing)
                    , (if (head $ cprob simslc) == 1.0
                       then Just CooperateOnLastInTheFaceOfCooperation
                       else Nothing)
                    , (if (head $ cprob simsld) == 1.0
                       then Just CooperateOnLastInTheFaceOfDefection
                       else Nothing)
                    , (if (head $ cprob simsuc) /= 1.0
                       then Just Uncompromising
                       else Nothing)
                    , (if    (    (head $ cprob simsd) > 0.0
                               && (head $ cprob simsd) < 1.0)
                          || (    (head $ cprob simsdd) > 0.0
                               && (head $ cprob simsdd) < 1.0)
                       then Just Nondeterministic
                       else Nothing)
                    , (if    (head $ head $ simsto) == Just Defect
                          && allthesame (head $ simsto)
                       then Just DefectOnTimeout
                       else Nothing)
                    , (if    (head $ head $ simsto) == Just Cooperate
                          && allthesame (head $ simsto)
                       then Just CooperateOnTimeout
                       else Nothing)
                    , (if    (head $ head $ simsto) == Nothing
                          && allthesame (head $ simsto)
                       then Just Timeout
                       else Nothing)
                    , (if    (head $ head $ simsr) == Just Defect
                       then Just Retaliating
                       else Nothing)
                    , (if    (head $ head $ simsf) == Just Cooperate
                       then Just Forgiving
                       else Nothing)
                    , (if simssi == [replicate 3 Nothing]
                       then Just Simulate
                       else Nothing)
                    , Nothing   -- Extortionist
--                    , (if    (head $ cprob simsdd) < 0.5
--                          && (head $ cprob simsec) > 0.80
--                          && (head $ cprob simsed) == 0.0
--                       then Just Extortionist
--                       else Nothing)
                    ]


-- Fingerprint the bot
fingerprint :: BotEnvironment m => Bot -> [Moves] -> Int -> m [Behavior]
fingerprint op _ r = do
     profile <- basicProfiler op r
     return (fmap (fromMaybe Nil) profile)
     -- return (fmap (fromMaybe Nil) (filter (/= Nothing) profile))

fingerprintCooperator :: [Behavior]
fingerprintCooperator = [UnconditionalCooperator,Nil,Nil,Nil,CooperateOnLastInTheFaceOfCooperation,CooperateOnLastInTheFaceOfDefection,Nil,Nil,Nil,CooperateOnTimeout,Nil,Nil,Forgiving,Nil,Nil]

fingerprintJusticeLike :: [Behavior]
fingerprintJusticeLike = [Nil,Nil,Nil,DefectOnLastInTheFaceOfDefection,CooperateOnLastInTheFaceOfCooperation,Nil,Uncompromising,Nil,DefectOnTimeout,Nil,Nil,Nil,Forgiving,Simulate,Nil]

fingerprintOccamLike :: [Behavior]
fingerprintOccamLike = [Nil,Nil,DefectOnLastInTheFaceOfCooperation,DefectOnLastInTheFaceOfDefection,Nil,Nil,Uncompromising,Nil,Nil,Nil,Timeout,Nil,Forgiving,Simulate,Nil]

-- "If they shoot and I don't, I'm screwed. If we both shoot, it hurts a
-- little. If we cooperate, it's Christmas for both of us. Except that there
-- is always an incentive to pull the trigger."
--                                     -- Hannu Rajaniemi, The Quantum Thief
-- Event space
moves :: [(Choice, Choice)]
moves = [(Cooperate, Cooperate), (Defect, Cooperate), (Cooperate, Defect), (Defect, Defect)]

-- Index of the maximum value in the list
maxIndex :: Ord b => [b] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

-- Expected value at decision node #0
ev0 :: Double -> Double -> Double
ev0 p ev = (p * ev) + ((1 - p) * fromIntegral(fst (payoff(Cooperate, Defect))))

-- Expected value at decision node #1
ev1 :: Num a => a -> Int -> a
ev1 p ord = p * fromIntegral(fst (payoff (moves !! ord)) + fst (payoff (Cooperate, Cooperate))) + (1 - p) * fromIntegral(fst (payoff (moves !! ord)) + fst (payoff (Cooperate, Defect)))

dPayoff :: Double
dPayoff = fromIntegral (fst $ payoff (Defect,Defect))

-- "Now, I return to this young fellow. And the communication I have
-- got to make is, that he has great expectations."
--                            -- Charles Dickens, Great Expectations
--
-- Great(est) Expectation
-- Pick the move that maxmizes the expected value from the
-- next two rounds. If the expected value is lower than the defection
-- payoff, defect.
-- greatestEV :: (Fractional a, Ord a) => [a] -> [a] -> [Moves] -> Choice
greatestEV :: [Double] -> [(Choice, b)] -> Choice
greatestEV ev0 moves = (if (maximum ev0 < dPayoff)
                            then Defect
                            else (fst (moves !! (maxIndex ev0))))

-- "A few million rounds more and I'll be a Boy Scout"
--               -- Hannu Rajaniemi, The Quantum Thief
--
-- Expected number of rounds
nrounds :: Int
nrounds = 100

-- Default NaN to 0.0
defaultNaN :: RealFloat a => a -> a
defaultNaN n = if isNaN n then 0.0 else n

-- Get the right simulation setup for the task at hand
-- Simulation step #0
simulator0 :: BotEnvironment m => [Behavior] -> Bot -> [[Moves]] -> m [[Maybe Choice]]
simulator0 fp op = case fp of
    val |  fingerprintJusticeLike == fp -> (simulate 25 10000 op helperCooperateBot)
        |         Simulate `notElem` fp -> (simulate 10 10000 op helperCooperateBot)
        |    DefectOnLastInTheFaceOfDefection `elem` fp
          && CooperateOnLastInTheFaceOfCooperation `elem` fp
          && Simulate `elem` fp -> (simulate 10 10000 op helperCooperateBot)
        |    Extortionist `elem` fp -> (simulate 25 15000 op helperTitForTatBot)
        |    UnconditionalCooperator `elem` fp
          && Nondeterministic `elem` fp
          && Forgiving `elem` fp
          && Timeout `elem` fp
          && Simulate `elem` fp -> (simulate 25 15000 op helperTitForTatBot)
        |    Uncompromising `elem` fp
          && Nondeterministic `notElem` fp
          && Simulate `elem` fp  -> (simulate 25 15000 op helperTitForTatBot)
        |    Nondeterministic `elem` fp
          && Simulate `elem` fp -> (simulate 37 15000 op helperRandomBot)
          -- && Simulate `elem` fp -> (simulate 70 15000 op helperRandomBot)
        | otherwise -> (simulate 25 15000 op helperTitForTatBot)

-- Frequency estimatation - step #0
fp0 :: RealFloat c => [Behavior] -> [[Maybe Choice]] -> c
fp0 fp = case fp of
    val |  fingerprintJusticeLike == fp -> (defaultNaN . head . cprob)
        |         Simulate `notElem` fp -> (defaultNaN . head . cprob)
        |    DefectOnLastInTheFaceOfDefection `elem` fp
          && CooperateOnLastInTheFaceOfCooperation `elem` fp
          && Simulate `elem` fp -> (defaultNaN . head . cprob)
        |    Extortionist `elem` fp -> (defaultNaN . head . cprob)
        |    UnconditionalCooperator `elem` fp
          && Nondeterministic `elem` fp
          && Forgiving `elem` fp
          && Timeout `elem` fp
          && Simulate `elem` fp -> (defaultNaN . head . cprob)
        |    Uncompromising `elem` fp
          && Nondeterministic `notElem` fp
          && Simulate `elem` fp  -> (defaultNaN . head . cprob)
        |    Nondeterministic `elem` fp
          && Simulate `elem` fp -> (defaultNaN . ((*) 2) . head . cprob)
        | otherwise -> (head . cprob)

-- Simulation step #1
simulator1 :: BotEnvironment m => [Behavior] -> Bot -> [[Moves]] -> m [[Maybe Choice]]
simulator1 fp op = case fp of
    val |  fingerprintJusticeLike == fp -> (simulate 25 10000 op helperCooperateBot)
        |         Simulate `notElem` fp -> (simulate 10 10000 op helperCooperateBot)
        |    DefectOnLastInTheFaceOfDefection `elem` fp
          && CooperateOnLastInTheFaceOfCooperation `elem` fp
          && Simulate `elem` fp -> (simulate 10 10000 op helperCooperateBot)
        |    Extortionist `elem` fp -> (simulate 25 15000 op helperTitForTatBot)
        |    UnconditionalCooperator `elem` fp
          && Nondeterministic `elem` fp
          && Forgiving `elem` fp
          && Timeout `elem` fp
          && Simulate `elem` fp -> (simulate 25 15000 op helperTitForTatBot)
        |    Uncompromising `elem` fp
          && Nondeterministic `notElem` fp
          && Simulate `elem` fp  -> (simulate 25 15000 op helperTitForTatBot)
        |    Nondeterministic `elem` fp
          && Simulate `elem` fp -> (simulate 27 15000 op helperRandomBot)
          -- && Simulate `elem` fp -> (simulate 34 15000 op helperRandomBot)
        | otherwise -> (simulate 25 15000 op helperTitForTatBot)

-- Frequency estimatation - step #1
fp1 :: RealFloat a => [Behavior] -> [[Maybe Choice]] -> [a]
fp1 fp = case fp of
    val |  fingerprintJusticeLike == fp -> ( (map defaultNaN) . cprob)
        |         Simulate `notElem` fp -> ( (map defaultNaN) . cprob)
        |    DefectOnLastInTheFaceOfDefection `elem` fp
          && CooperateOnLastInTheFaceOfCooperation `elem` fp
          && Simulate `elem` fp -> ( (map defaultNaN) . cprob)
        |    Extortionist `elem` fp -> ( (map defaultNaN) . cprob)
        |    UnconditionalCooperator `elem` fp
          && Nondeterministic `elem` fp
          && Forgiving `elem` fp
          && Timeout `elem` fp
          && Simulate `elem` fp -> ( (map defaultNaN) . cprob)
        |    Uncompromising `elem` fp
          && Nondeterministic `notElem` fp
          && Simulate `elem` fp  -> ((map defaultNaN) . cprob)
        |    Nondeterministic `elem` fp
          && Simulate `elem` fp -> ( (map defaultNaN) . (map (* 2)) . cprob)
        | otherwise -> ((map defaultNaN) . cprob)


-- Find out if the opponent is a replica
handshake :: BotEnvironment m => Bot -> Int -> Int -> [Behavior] -> m Bool
handshake op l r fp = if l >= r
                      then return False
                      else if fp /= fingerprintOccamLike
                           then return False
                           else do
                                s1 <- simulate 1 15000 op helperDefectBot
                                            [replicate r (Defect,Defect)]
                                s2 <- simulate 1 15000 op helperDefectBot
                                  [replicate (r+1) (Cooperate,Cooperate)]
                                return (  (head $ head s1) == Just Cooperate
                                        &&(head $ head s2) == Just Defect)

--
-- Pip Bot
--
pipBot :: Bot
pipBot = Bot (\op hist -> do
             let len = length hist
             -- Does the opponent have good manners?
             fp <- fingerprint op hist nrounds
             hs <- handshake op len nrounds fp
             let pipl = fp == fingerprintOccamLike
             case len of
                     -- "The All-Defector. The thing that never cooperates and
                     -- gets away with it. It found a glitch in the system so
                     -- that it always appears as you. And if you can't trust
                     -- yourself, who can you trust?"
                     --                  -- Hannu Rajaniemi, The Quantum Thief
                 val | pipl && hs -> return Cooperate
                     | val == nrounds - 1
                       && (DefectOnLastInTheFaceOfCooperation `elem` fp
                           || CooperateOnLastInTheFaceOfDefection `elem` fp) ->
                         return Defect
                     -- "The warmind raises its weapon slowly. A ripple passes
                     -- through its eyestalks. I wish it had a face: the stare
                     -- of its moist forest of orbs is unnerving. Never mind.
                     -- It's going to work this time. I tilt the gun upwards
                     -- slightly, my body language and wrist movement
                     -- suggesting the motion I would make it I was going
                     -- to put up my gun. My every muscle screams cooperation.
                     -- Come on. Fall for it. Honest. This time, we are going
                     -- to be friends ---"
                     --                  -- Hannu Rajaniemi, The Quantum Thief
                     | fp == fingerprintCooperator -> return Cooperate
                     | pipl && val  > nrounds + 1 -> return Defect
                     | pipl && val == nrounds + 1 ->
                         return (if   head hist == (Cooperate, Cooperate)
                                   && allthesame hist
                                 then Defect
                                 else Cooperate)
                     | val == nrounds     ->
                         return (if   head hist == (Defect, Defect)
                                   && allthesame hist
                                 then Cooperate
                                 else Defect)
                     | otherwise -> do
                       -- "The Prison is all about education. And game theory:
                       -- the mathematics of rational decision-making. When
                       -- you are an immortal mind like the Archons, you have
                       -- time to be obsessed with such things. And it is just
                       -- like the Sobornost --- the upload collective that
                       -- rules the Inner Solar System --- to put them in
                       -- charge of their prisons."
                       --                -- Hannu Rajaniemi, The Quantum Thief
                       --
                       -- Look one step ahead
                       -- How the opponent would behave against a cooperator
                       -- given the history of the previous rounds?
                       sims0 <- (simulator0 fp op) [invert hist]
                       let p = (fp0 fp) sims0
                       -- "And this is good matter to turn into a Prison. Its
                       -- mouth waters in anticipation of the taste of patterns
                       -- that the iterated Dilemmas will make. Its copyfather
                       -- discovered a defector pattern that tastes like pecan
                       -- ice cream: a replicating strategy family like a flyer
                       -- in a Game of Life."
                       --                 -- Hannu Rajaniemi, The Quantum Thief
                       --
                       -- Look ahead one more round
                       -- How the same opponent would react to each
                       -- possible move?
                       let futures = [hist ++ [move] | move <- moves]
                       sims1 <- (simulator1 fp op) (map invert futures)
                       let ps = if length hist < (nrounds - 1)
                                then (fp1 fp) sims1
                                else (replicate 4 0.0)
                       -- Compute the expected value for each possible move
                       -- Decision tree rollback
                       let eval1 = zipWith (ev1) ps [0..3]
                       let eval0 = zipWith (ev0) [p,p,p,p] eval1
                       -- "The engines in her mind prune the game-theoretic
                       -- branches like diamond-bladed chainsaws. There are
                       -- many paths through this, like meanings in an Oortian
                       -- song, and she only needs to find one ---"
                       --               --  Hannu Rajaniemi, The Quantum Thief
                       -- Pick the move that maxmizes the expected value from
                       -- the next two rounds.
                       return (greatestEV eval0 moves))

-- "We stare at each other for a moment. I feel stupid. Too long on the train
-- of defection and cooperation. Time to jump off. I'm the first one to look
-- away."
--                                     --  Hannu Rajaniemi, The Quantum Thief

-- "I distract myself by looking at the view. My wall has enough magnification
-- to show the Dilemma Prison in the distance. It's a diamonoid torus almost
-- a thousand kilometers in diameter, but from this angle it looks like a
-- glistening slit-pupilled eye among the stars, staring straight at me. I
-- swallow and blink it away."
--                                     --  Hannu Rajaniemi, The Quantum Thief
