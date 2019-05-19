module Waltz where

import           Data.Tree
import           PrefixTree
import           ScalePitch

firstMeasureRightHand :: Tree (TreeModifier -> TreeModifier)
firstMeasureRightHand = Node
  (atChords [0, 1, 2, 3] . atVoices [0, 1])
  [ leaf
    ( setScale (extractTriad 0 $ mkMajorScale 8)
    . setDuration (3 / 8)
    . setVolume 100
    . setOctave 4
    . setDegree 1
    )
  , leaf (atChords [1, 2, 3] . setDuration (1 / 8) . modifyVolume (subtract 20))
  , leaf (atChords [1, 2] . modifyAbsDegree (subtract 1))
  , leaf
    (atVoices [1] . modifyAbsDegree (subtract 2) . modifyVolume (subtract 20))
  ]

firstMeasure :: Tree (TreeModifier -> TreeModifier)
firstMeasure = Node
  (setScale (extractTriad 0 $ mkMajorScale 8))
  [ Node (atHands [0]) [firstMeasureRightHand]
    -- Left hand
  , Node
    (atHands [1] . setDuration (1 / 4) . setVolume 50)
    [ leaf (atChords [0] . atVoices [0] . setOctave 2 . setDegree 0)
    , Node
      (atChords [1, 2] . atVoices [0, 1, 2] . setOctave 3 . setDegree 1)
      [ leaf id
      , leaf (atChords [2] . modifyAbsDegree (subtract 1))
      , leaf (atVoices [1] . modifyAbsDegree (subtract 1))
      , leaf (atVoices [2] . modifyAbsDegree (subtract 2))
      ]
    ]
  ]

firstTwoMeasures = Node
  (atMeasures [0, 1] . setScale (extractTriad 0 $ mkMajorScale 8))
  [ Node (atHands [0]) [firstMeasureRightHand]
    -- Left hand
  , Node
    (atHands [1] . setDuration (1 / 4) . setVolume 60)
    [ Node
      (atChords [0] . atVoices [0] . setDegree 0)
      [leaf (atMeasures [0] . setOctave 2), leaf (atMeasures [1] . setOctave 1)]
    , Node
      (atChords [1, 2] . atVoices [0, 1, 2] . setOctave 3 . setDegree 1)
      [ leaf id
      , leaf (atChords [2] . modifyAbsDegree (subtract 1))
      , leaf (atVoices [1] . modifyAbsDegree (subtract 1))
      , leaf (atVoices [2] . modifyAbsDegree (subtract 2))
      ]
    ]
  ]

firstPhrase = Node
  id
  [ Node
    ( atMeasures [2]
    . setScale (mkMajorScale 8)
    . setDuration (1 / 3)
    . setVolume 100
    )
    [ Node
        (atHands [0])
        [ Node
          (atChords [0])
          [ Node
            ( atVoices [0, 1, 2]
            . modifyScale (extractTriad 3)
            . setOctave 5
            . setDegree 0
            )
            [ leaf (atVoices [0] . setDuration (1 / 6))
            , leaf (atVoices [1] . modifyAbsDegree (subtract 1))
            , leaf (atVoices [2] . modifyAbsDegree (subtract 2))
            ]
          , Node
            (atVoices [0] . atNotes [1, 2] . setDuration (1 / 12))
            [ leaf (atNotes [1] . setOctave 4 . setDegree 4)
            , leaf (atNotes [2] . setOctave 4 . setDegree 3)
            ]
          ]
        , Node
          (atChords [1] . modifyScale (extractTriad 0))
          [ leaf (atVoices [0, 1, 2] . setOctave 4 . setDegree 1)
          , leaf (atVoices [1] . modifyAbsDegree (subtract 1))
          , leaf (atVoices [2] . modifyAbsDegree (subtract 2))
          ]
        , Node
          (atChords [2] . modifyScale (extractSeventh 1))
          [ leaf (atVoices [0, 1, 2] . setOctave 4 . setDegree 0)
          , leaf (atVoices [1] . modifyAbsDegree (subtract 1))
          , leaf (atVoices [2] . modifyAbsDegree (subtract 3))
          ]
        ]
    ]
  , leaf
    (atMeasures [2] . atChords [0, 1, 2] . atVoices [1, 2] . modifyVolume
      (subtract 20)
    )
  ]


