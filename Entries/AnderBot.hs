module Entries.AnderBot where

import Tournament
import Bots

anderBot :: Bot
anderBot = Bot run
  where run op hist
            | null hist        = return Cooperate
            | length hist < 11 = runBot titForTatBot op hist
            | otherwise        = do
                random <- rand
                if random < 0.1 || (opDefections hist > myDefections hist)
                    then return Defect
                    else runBot smarterMirrorBot op hist
        myDefections = length . filter (== Defect) . map fst
        opDefections = length . filter (== Defect) . map snd
