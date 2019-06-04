module Waltz.PhraseC where

import           Event
import           Tree
import           Waltz.Shared

phraseC :: MusicTree
phraseC = Node
  (atPhrases [2])
  [ -- Measures 0, 1, 2, 3, 4
    Node
    (atMeasures [0, 1, 2, 3, 4])
    [ -- Harmony for both hands
      Node
      (atHands [0, 1])
      [ Leaf (atMeasures [0]) (setScale (extractSeventh 4 (mkMajorScale 8)))
      , Leaf (atMeasures [1]) (setScale (extractTriad 0 (mkMajorScale 8)))
      , Leaf (atMeasures [2]) (setScale (extractSeventh 4 (mkMajorScale 1)))
      , Leaf (atMeasures [3]) (setScale (extractTriad 0 (mkMajorScale 1)))
      , Leaf (atMeasures [4]) (setScale (extractSeventh 4 (mkMajorScale 3)))
      ]
      -- Right hand
    , Node
      (atHands [0])
      [ unevenRhythmMeter
      , Node
        (atMeasures [0, 2])
        [ Leaf id (setOctave 5 . setDegree 0)
        , voiceStack (atChords [0]) id [-1, -2, -3, -4]
        , voiceStack (atChords [1, 2]) (modifyAbsDegree (subtract 2)) [-1, -2]
        , voiceStack (atChords [3]) id [-1]
        ]
      , Node
        (atMeasures [1, 3])
        [ Leaf id               (setOctave 4 . setDegree 2)
        , Leaf (atMeasures [3]) (modifyOctave (+ 1))
        , voiceStack (atChords [0]) id [-1, -2, -3]
        , voiceStack (atMeasures [1] . atChords [1, 2])
                     (modifyAbsDegree (subtract 1))
                     [-1]
        , voiceStack (atMeasures [3] . atChords [1, 2])
                     (modifyAbsDegree (subtract 1))
                     [-1, -2]
        , Leaf (atChords [3]) id
        ]
      , Node
        (atMeasures [4])
        [ Leaf id (setOctave 5 . setDegree 0)
        , voiceStack (atChords [0]) id [-1, -2, -3, -4]
        , voiceStack (atChords [1, 2]) (modifyAbsDegree (subtract 2)) [-1, -2]
        , Leaf (atChords [3]) (modifyAbsDegree (subtract 1))
        ]
      ]
      -- Left hand
    , Node
      (atHands [1])
      [ evenRhythmMeterL
      , Node
        (atMeasures [0, 2])
        [ Node
          (atVoices [0])
          [ Leaf (atMeasures [0]) (setDegree 0 . setOctave 2)
          , Leaf (atMeasures [2]) (setDegree 0 . setOctave 1)
          ]
        , voiceStack (atChords [1]) (setOctave 3 . setDegree 3) [-2, -3]
        , voiceStack (atChords [2]) (setOctave 3 . setDegree 1) [-1]
        ]
      , Node
        (atMeasures [1, 3])
        [ Leaf (atVoices [0]) (setOctave 2 . setDegree 0)
        , voiceStack (atChords [1]) (setOctave 3 . setDegree 1) [-1, -2]
        , voiceStack (atChords [2]) (setOctave 3 . setDegree 0) [-1]
        , Leaf (atMeasures [3]) (modifyOctave (+ 1))
        ]
      , Node
        (atMeasures [4])
        [ voiceStack (atVoices [0]) (setOctave 2 . setDegree 0) [-4]
        , voiceStack (atChords [1]) (setOctave 3 . setDegree 2) [-1, -2, -3]
        , voiceStack (atChords [2]) (setOctave 3 . setDegree 1) [-1, -2]
        ]
      ]
    ]
    -- Measure 5
  , Node
    (atMeasures [5])
    [ Leaf id (setDuration (1 / 4))
      -- Right hand
    , Node
      (atHands [0])
      [ voiceStack
        (atChords [0, 1, 2])
        ( setScale (extractSeventh 1 $ mkMajorScale 8)
        . setVolume 100
        . setOctave 4
        . setDegree 3
        )
        [-2, -3]
      , Node
        (atChords [0] . atVoices [0])
        [ Leaf (atNotes [0, 1]) (setDuration (1 / 8))
        , Leaf (atNotes [1])    (modifyScalePitch (stepDown (mkMajorScale 8)))
        ]
      , Node
        (atChords [1, 2])
        [ Leaf (atVoices [0]) (modifyScalePitch (stepDown (mkMajorScale 8)))
        , Leaf
          id
          (modifyScalePitch (roundDown (extractTriad 3 $ mkMajorScale 8)))
        ]
      , Leaf
        (atChords [2])
        (modifyScalePitch (roundDown (extractSeventh 4 $ mkMajorScale 8)))
      ]
      -- Left hand
    , Node
      (atHands [1])
      [ Leaf id (setVolume 60 . setScale (extractSeventh 4 $ mkMajorScale 8))
      , Leaf (atChords [0]) (setOctave 2 . setDegree 0)
      , voiceStack (atChords [1]) (setOctave 3 . setDegree 3) [-1, -3]
      , voiceStack (atChords [2]) (setOctave 4 . setDegree 0) [-2, -4]
      ]
    ]
  ]
