module Waltz.Full where

import           Event
import           Tree

import           Waltz.Shared
import           Waltz.PhraseA
import           Waltz.PhraseB
import           Waltz.PhraseC

-- Brahms's Waltz Op 39 No 15, excluding the last period
waltz :: MusicTree
waltz = Node
  (atPeriods [0, 1, 2])
  [ Leaf (atVoices [0]) (modifyVolume (+ 10))
  , Node
    (atPeriods [0, 1, 2])
    [Node (atPhrases [0]) [phraseA], Node (atPhrases [1]) [phraseBAllPeriods]]
  , Node (atPeriods [1, 2] . atPhrases [2]) [phraseC]
  ]
