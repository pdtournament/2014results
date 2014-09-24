module Entries.TatForTit where

import Tournament
import Bots

tatForTit :: Bot
tatForTit = Bot (\op hist -> case (reverse hist) of
    [] -> runBot op tatForTit (invert $ reverse((Cooperate,Cooperate):(reverse hist)))
    y:(x:xs) -> (case ((fst y) == (snd x)) of
        False -> return Defect
        True -> (case (3+(length xs)) of
            100 -> return (fst y)
            _ -> runBot op tatForTit (invert $reverse((fst y,Cooperate):(reverse hist)))))
    y:xs -> (case ((snd y)==Defect) of
        True -> return Defect
        False -> runBot op tatForTit (invert $reverse((fst y,Cooperate):(reverse hist)))))
