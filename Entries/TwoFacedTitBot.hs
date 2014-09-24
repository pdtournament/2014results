module Entries.TwoFacedTitBot where

import Tournament
import Bots

-- Simulate my opponent playing a round against MirrorBot
-- if my opponent would take more than 10ms against MirrorBot
-- (i.e., they're probably trying to simulate me against themselves), Cooperate
-- Otherwise, play like a TitForTatBot who always defects on the last round
twoFacedTitBot :: Bot
twoFacedTitBot = Bot (\op hist -> do
    simulation <- time 10000 . runBot op mirrorBot $ invert hist
    return (case simulation of
                Nothing   -> Cooperate
                Just move -> (if hist == []
							  then Cooperate
							  else (if (length hist) == 99 then Defect else snd $ last hist)
							  )))

newExamplePlayers = [ Player "CooperateBot" cooperateBot
                 , Player "DefectBot" defectBot
                 , Player "TitForTatBot" titForTatBot
                 , Player "SmarterMirrorBot" smarterMirrorBot
                 , Player "JusticeBot" justiceBot]

runNewExample = displayTournament 10 ((Player "TwoFacedTitBot" twoFacedTitBot):newExamplePlayers)
