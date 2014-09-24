module Entries.CheeseBot where
-- The bot I'm submitting is called CheeseBot; see the function "cheeseBot"
-- at the bottom of the file.

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)

import Tournament
import Bots

-- An operator to combine multiple tests in sequence. The next test is only
-- executed if the previous one failed; otherwise the result should be
-- short-circuited.
infixl 1 >>>
(>>>) :: (BotEnvironment m, Eq a) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
m1 >>> m2 = do
    r1 <- m1
    if (r1 == Nothing)
        then m2
        else (return r1)

--------------------------- Basic functions --------------------------
-- The number of rounds in the tournament (should be 100)
numRounds :: Int
numRounds = 100

-- A simple check for whether the current round is the last.
isLastRound :: [Moves] -> Bool
isLastRound hist = (length hist >= numRounds - 1)

------------------------- Simple history checks -------------------------
-- Always punish someone who defected against us last round.
retaliate :: [Moves] -> (Maybe Choice)
retaliate hist
    | not (null hist) && (snd . last $ hist) == Defect =
        Just Defect
    | otherwise =
        Nothing

-- Always cooperate on the last round.
coopAtEnd :: [Moves] -> (Maybe Choice)
coopAtEnd hist
    | isLastRound hist = Just Cooperate
    | otherwise        = Nothing

--------------------- Test the other bot via simulations ---------------------
-- If our opponent seems to be unwilling to punish defectors at all, or not
-- strongly enough, we should defect against them.
defectVsNaive :: BotEnvironment m => Bot -> [Moves] -> m (Maybe Choice)
defectVsNaive opp hist
    -- If it's the last round we check as though it were the first.
    | isLastRound hist = do
        test <- runBot opp defectBot []
        if (test == Cooperate)
            then return (Just Defect)
            else return Nothing
    -- If it's not the last round we look 1 round ahead.
    | otherwise        = do
        let histC = (hist ++ [(Cooperate, Cooperate)])
        let histD = (hist ++ [(Defect, Cooperate)])
        testC <- replicateM 8 . runBot opp cooperateBot $ invert histC
        testD <- replicateM 8 . runBot opp defectBot $ invert histD
        let nCvC = length . filter (== Cooperate) $ testC
        let nCvD = length . filter (== Cooperate) $ testD
        return $ naiveCheck nCvC nCvD

-- A check to see if the opponent is too stupid to punish defectors properly.
-- nCvC = number of cooperates out of 8 vs CooperateBot
-- nCvD = number of cooperates out of 8 vs DefectBot
naiveCheck :: Int -> Int -> (Maybe Choice)
naiveCheck nCvC nCvD
    -- If they never cooperate with DefectBot they're probably not too naive.
    | nCvD == 0              = Nothing
    -- If they cooperate vs cooperators significantly more often than vs
    -- defectors, they're probably not random enough for an easy Defect.
    | (nCvC - nCvD) >= 4     = Nothing
    -- Free utilons!
    | otherwise              = Just Defect

------------- Self-referential simulations. Be wary of recursion! -------------
-- If we're under a genuine threat of punishment, we cooperate.
-- Recursion is avoided via reference to the future, which must end with the
-- last round.
-- Since this could potentially involve ~2^100 operations vs. a bot that does
-- a direct simulation, the check is performed as though the current round were
-- round #96 or later.
-- A time limit is also used to make this check more viable vs bots that could
-- potentially take a long time.
coopVsThreat :: BotEnvironment m => Bot -> Bot -> [Moves] -> m (Maybe Choice)
coopVsThreat self opp hist
    | isLastRound hist = return Nothing
    | otherwise        = do
        -- Extend the past so it's as though there's only 5 rounds left.
        -- That way there's enough time to simulate future possibilities.
        let hist2 = if (length hist < (numRounds - 5))
            then ((replicate (numRounds - 5 - (length hist)) (Cooperate, Cooperate)) ++ hist)
            else hist
        testC <- time 100000 . runBot opp self $ invert (hist2 ++ [(Cooperate, Cooperate)])
        testD <- time 100000 . runBot opp self $ invert (hist2 ++ [(Defect, Cooperate)])
        if (testC == Just Cooperate && testD == Just Defect)
            then return (Just Cooperate)
            else return Nothing

----------- Some functionality to detect MirrorBot-like opponents -----------
-- If our opponent is a rock (CooperateBot or DefectBot), we defect vs them.
defectVsRock :: BotEnvironment m => Bot -> [Moves] -> m (Maybe Choice)
defectVsRock opp hist = do
    -- Check if they cooperate against DefectBot 3x in a row.
    d1 <- runBot opp defectBot []
    d2 <- runBot opp defectBot [(d1, Defect)]
    d3 <- runBot opp defectBot [(d2, Defect)]
    -- Check if they defect against TitForTatBot 3x in a row.
    tft1 <- runBot opp titForTatBot []
    tft2 <- runBot opp titForTatBot [(tft1, Cooperate)]
    tft3 <- runBot opp titForTatBot [(tft2, Defect)]
    if (all (== Cooperate) [d1, d2, d3] || all (== Defect) [tft1, tft2, tft3])
        then return (Just Defect)
        else return Nothing

-- A simple bot that will defect vs rocks.
simpleBot = Bot $ \opp hist -> fmap (fromMaybe Cooperate) $
    defectVsRock opp hist

-- If we're under threat of punishment this very round, we cooperate.
coopVsMirror :: BotEnvironment m => Bot -> [Moves] -> m (Maybe Choice)
coopVsMirror opp hist = do
    testC <- runBot opp simpleBot $ invert hist
    testD <- runBot opp defectBot $ invert hist
    if (testC == Cooperate && testD == Defect)
        then return (Just Cooperate)
        else return Nothing

-------------------------- The real CheeseBot! --------------------------
cheeseBot :: Bot
cheeseBot = Bot $ \opp hist -> fmap (fromMaybe Defect) $
    -- Always retaliate, TFT-style.
    return (retaliate hist) >>>
    -- Defect vs opponents who are too naive to punish defectors properly.
    defectVsNaive opp hist >>>

    -- Self-referential checks!!
    -- Cooperate if we are under threat of punishment in the future.
    coopVsThreat cheeseBot opp hist >>>

    -- Since we may run into MirrorBot-like bots, it would be better to
    -- cooperate vs them.
    -- Unfortunately there isn't any particularly robust approach to
    -- doing this due to the potential for infinite recursion.
    -- In this case, I've chosen to detect opponents who defect vs
    -- DefectBot but are willing to cooperate with SimpleBot.
    -- This makes us somewhat exploitable to opponents who cooperate with
    -- SimpleBot, but this seems like the best bet to make.
    coopVsMirror opp hist >>>
    -- Cooperate by default on the last round, so that the defection threat
    -- is always credible.
    return (coopAtEnd hist)
