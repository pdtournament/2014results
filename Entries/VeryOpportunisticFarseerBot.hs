module Entries.VeryOpportunisticFarseerBot where

import Tournament
import Bots

hasAnybodyEverDefected history = not $ foldl (\acc x -> acc && (x == (Cooperate,Cooperate))) True history

numRounds = 100
lastRoundId = numRounds - 1

backstabberFunc n otherBot history =
	if (length history) == (numRounds - n)
		then return Defect
		else case history of
            		[] -> return Cooperate
            		xs -> return . snd $ last xs

backstabberBot :: Bot
backstabberBot = Bot(backstabberFunc 1)

veryOpportunisticFarseerNextRoundFunc selfBot otherBot history timeout0 timeout1 = do
	simulation <- time timeout0 . runBot otherBot selfBot $ ((invert history) ++ [(Cooperate, Cooperate)])
	simulation2 <- time timeout1 . runBot otherBot selfBot $ ((invert history) ++ [(Cooperate, Defect)])
	return (case simulation of
       	        Just Cooperate -> (case simulation2 of
       	        			Just Cooperate   -> Defect
       	        			otherwise -> Cooperate)
       	        otherwise -> Defect)

veryOpportunisticFarseerLastRoundFunc otherBot history timeout0 timeout1 =
	let iih = invert (init history) in do
		simulation <- time timeout0 . runBot otherBot backstabberBot $ iih
		simulation2 <- time timeout1 . runBot otherBot backstabberBot $ (iih ++ [(Cooperate, Cooperate)])
		return (if (simulation == Just Cooperate) && (simulation2 == Just Cooperate)
			then Defect
			else Cooperate)

veryOpportunisticFarseerBot :: Bot
veryOpportunisticFarseerBot = Bot(\otherBot history ->
	if hasAnybodyEverDefected history
		then return Defect
		else if (length history == lastRoundId)
			then veryOpportunisticFarseerLastRoundFunc otherBot history 10000 10000
			else veryOpportunisticFarseerNextRoundFunc veryOpportunisticFarseerBot otherBot history 4000000 10000)



