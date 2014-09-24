module Entries.SwitchBot where

import Bots hiding (mirrorBot)
import Tournament


--this is the bot for the tournament
switchBot :: Bot
switchBot = Bot (\op hist -> case hist of
        [] -> return Cooperate
        [(Cooperate,Defect)] -> do
                              m <- runBot op mirrorBot $ invert hist
                              return m
        xs -> return . snd $ last xs)


--original mirrorBot as helper function
mirrorBot = Bot (\op hist -> runBot op mirrorBot $ invert hist)
