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
  id
  [ Node
    (atPeriods [0, 1, 2, 3])
    [ Node (atPhrases [0])                    [phraseAPeriods0123]
    , Node (atPhrases [1])                    [phraseBPeriods0123]
    , Node (atPeriods [1, 2] . atPhrases [2]) [phraseC]
    ]
  ]
