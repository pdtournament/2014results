module Entries.SimpleTFTseerBot where

import Tournament
import Bots

hasOtherEverDefected history = (elem Defect $ map snd history)
hasAnybodyEverDefected history = not $ foldl (\acc x -> acc && (x == (Cooperate,Cooperate))) True history

numRounds = 100
lastRoundId = numRounds - 1

simpleTFTseerBotFunc selfBot otherBot history timeout = do
	simulation <- time timeout . runBot otherBot selfBot $ ((invert history) ++ (replicate (lastRoundId - (length history)) (Cooperate, Cooperate)))
	return (case simulation of
       	        Just Cooperate -> Cooperate
       	        otherwise -> Defect)

simpleTFTseerBot :: Bot
simpleTFTseerBot = Bot(\otherBot history ->
	if hasAnybodyEverDefected history
		then return Defect
		else if (length history == lastRoundId)
			then return Cooperate
			else simpleTFTseerBotFunc simpleTFTseerBot otherBot history 4000000)



