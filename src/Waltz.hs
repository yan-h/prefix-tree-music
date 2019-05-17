module Waltz where

import           Data.Tree
import           PrefixTree
import           ScalePitch

stack3 :: Tree (PartialTree -> PartialTree)
stack3 = Node
  id
  [ leaf (atVoices [1] . overAbsDegree (subtract 1))
  , leaf (atVoices [2] . overAbsDegree (subtract 2))
  ]

firstMeasureRightHand :: Tree (PartialTree -> PartialTree)
firstMeasureRightHand = Node
  (atChords [0, 1, 2, 3] . atVoices [0, 1] . setScale
    (extractTriad 0 $ mkMajorScale 8)
  )
  [ leaf (atVoices [1] . overAbsDegree (subtract 2))
  , leaf (atChords [0] . setDuration (3 / 8) . setVolume 100)
  , leaf (atChords [1, 2, 3] . setDuration (1 / 8) . setVolume 80)
  , leaf (atChords [0, 3] . setOctave 5 . setDegree 1)
  , leaf (atChords [1, 2] . setOctave 5 . setDegree 0)
  ]

firstMeasure :: Tree (PartialTree -> PartialTree)
firstMeasure = Node
  (setScale (extractTriad 0 $ mkMajorScale 8))
  [ Node
    (atHands [0] . atChords [0, 1, 2, 3] . atVoices [0, 1])
    [ leaf (atVoices [1] . overAbsDegree (subtract 2))
    , leaf (atChords [0] . setDuration (3 / 8) . setVolume 100)
    , leaf (atChords [1, 2, 3] . setDuration (1 / 8) . setVolume 80)
    , leaf (atChords [0, 3] . setOctave 5 . setDegree 1)
    , leaf (atChords [1, 2] . setOctave 5 . setDegree 0)
    ]
  , Node
    (atHands [1] . setDuration (1 / 4) . setVolume 60)
    [ leaf (atChords [0] . atVoices [0] . setOctave 2 . setDegree 0)
    , Node
      (atChords [1, 2] . atVoices [0, 1, 2])
      [ leaf (atChords [1] . setOctave 3 . setDegree 1)
      , leaf (atChords [2] . setOctave 3 . setDegree 0)
      , leaf (atVoices [1] . overAbsDegree (subtract 1))
      , leaf (atVoices [2] . overAbsDegree (subtract 2))
      --, stack3
      ]
    ]
  ]


firstTwoMeasures :: Tree (PartialTree -> PartialTree)
firstTwoMeasures = Node
  (atMeasures [0, 1] . setScale (extractTriad 0 $ mkMajorScale 8))
  [ Node
    (atHands [0] . atChords [0, 1, 2, 3] . atVoices [0, 1])
    [ leaf (atVoices [1] . overAbsDegree (subtract 2))
    , leaf (atChords [0] . setDuration (3 / 8) . setVolume 100)
    , leaf (atChords [1, 2, 3] . setDuration (1 / 8) . setVolume 80)
    , leaf (atChords [0, 3] . setOctave 5 . setDegree 1)
    , leaf (atChords [1, 2] . setOctave 5 . setDegree 0)
    ]
  , Node
    (atHands [1] . setDuration (1 / 4) . setVolume 60)
    [ Node
      (atChords [0] . atVoices [0] . setDegree 0)
      [leaf (atMeasures [0] . setOctave 2), leaf (atMeasures [1] . setOctave 1)]
    , Node
      (atChords [1, 2] . atVoices [0, 1, 2])
      [ leaf (atChords [1] . setOctave 3 . setDegree 1)
      , leaf (atChords [2] . setOctave 3 . setDegree 0)
      , leaf (atVoices [1] . overAbsDegree (subtract 1))
      , leaf (atVoices [2] . overAbsDegree (subtract 2))
      --, stack3
      ]
    ]
  ]

firstPhrase :: Tree (PartialTree -> PartialTree)
firstPhrase = Node
  (atMeasures [0, 1] . setScale (extractTriad 0 $ mkMajorScale 8))
  [ Node
    (atHands [0] . atChords [0, 1, 2, 3] . atVoices [0, 1])
    [ leaf (atVoices [1] . overAbsDegree (subtract 2))
    , leaf (atChords [0] . setDuration (3 / 8) . setVolume 100)
    , leaf (atChords [1, 2, 3] . setDuration (1 / 8) . setVolume 80)
    , leaf (atChords [0, 3] . setOctave 5 . setDegree 1)
    , leaf (atChords [1, 2] . setOctave 5 . setDegree 0)
    ]
  , Node
    (atHands [1] . setDuration (1 / 4) . setVolume 60)
    [ Node
      (atChords [0] . atVoices [0] . setDegree 0)
      [leaf (atMeasures [0] . setOctave 2), leaf (atMeasures [1] . setOctave 1)]
    , Node
      (atChords [1, 2] . atVoices [0, 1, 2])
      [ leaf (atChords [1] . setOctave 3 . setDegree 1)
      , leaf (atChords [2] . setOctave 3 . setDegree 0)
      , leaf (atVoices [1] . overAbsDegree (subtract 1))
      , leaf (atVoices [2] . overAbsDegree (subtract 2))
      --, stack3
      ]
    ]
  ]