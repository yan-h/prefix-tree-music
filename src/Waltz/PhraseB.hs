module Waltz.PhraseB where

import           Event
import           Tree
import           Waltz.Shared

-- Measures 0, 1. Both are identical.
phraseBMeasures01 :: MusicTree
phraseBMeasures01 = Node
  (atPhrases [1] . atMeasures [0, 1])
  [ Leaf id (setScale (mkMajorScale 8))
    -- Right hand
  , Node
    (atHands [0] . atChords [0, 1, 2, 3])
    [ unevenRhythmMeter
    ,  -- Chord 0
      voiceStack (atChords [0])
                 (modifyScale (extractTriad 5) . setOctave 5 . setDegree 0)
                 [-1, -2]
    , Leaf (atChords [1, 2, 3]) (modifyScale (extractTriad 0))
    , voiceStack (atChords [1, 2]) (setOctave 4 . setDegree 1) [-1]
    , Leaf (atChords [3] . atVoices [0]) (setOctave 4 . setDegree 2)
    ]
    -- Left hand
  , Node
    (atHands [1] . atChords [0, 1, 2])
    [ evenRhythmMeterL
    , Node
      (atChords [0, 1])
      [ Leaf id                            (modifyScale (extractTriad 5))
      , Leaf (atChords [0] . atVoices [0]) (setOctave 2 . setDegree 0)
      , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-1, -2, -3]
      ]
    , voiceStack (atChords [2])
                 (modifyScale (extractTriad 0) . setOctave 3 . setDegree 1)
                 [-2, -4]
    ]
  ]

-- Measure 2, with the right hand done using voice leading functions.
phraseBMeasure2Periods01 :: MusicTree
phraseBMeasure2Periods01 = Node
  (atPhrases [1] . atMeasures [2])
  [ -- Right hand
    Node
    (atHands [0])
    [ Leaf id (setDuration (1 / 4) . setVolume 100)
    , voiceStack
      (atChords [0, 1, 2])
      (setScale (extractTriad 2 $ mkMajorScale 8) . setOctave 5 . setDegree 2)
      [-2, -3]
    , Node
      (atChords [0] . atVoices [0])
      [ Leaf (atNotes [0, 1]) (setDuration (1 / 8))
      , Leaf (atNotes [1])    (modifyScalePitch (stepDown (mkMajorScale 8)))
      ]
    , Node
      (atChords [1, 2])
      [ Leaf (atVoices [0]) (modifyScalePitch (stepDown (mkMajorScale 8)))
      , Leaf id (modifyScalePitch (roundDown (extractTriad 2 $ mkMajorScale 8)))
      ]
    , Leaf (atChords [2])
           (modifyScalePitch (stepDown (extractSeventh 4 $ mkMajorScale 0)))
    ]
  , Node
    (atHands [1])
    [ Leaf id (setScale (mkMajorScale 8))
    , evenRhythmMeterL
      -- Chord 0
    , Leaf (atChords [0])
           (modifyScale (extractTriad 2) . setOctave 2 . setDegree 1)
      -- Chord 1
    , voiceStack (atChords [1])
                 (modifyScale (extractTriad 2) . setOctave 4 . setDegree 0)
                 [-1, -2]

      -- Chord 2
    , voiceStack
      (atChords [2])
      (setScale (extractSeventh 4 (mkMajorScale 0)) . setOctave 3 . setDegree 1)
      [-2, -6]
    ]
  ]

-- The complete second phrase in period 0, with the third measure pasted in.
phraseBPeriod0 :: MusicTree
phraseBPeriod0 = Node
  (atPhrases [1] . atPeriods [0])
  [ Leaf id (setScale (mkMajorScale 8))
  , phraseBMeasure2Periods01
  , Node
    (atMeasures [0, 1, 3])
    [ unevenRhythmMeter
    , evenRhythmMeterL
      -- Measure 3
    , Node
      (atMeasures [3])
      [ Leaf id (modifyScale (extractTriad 2))
        -- Right hand
      , Node
        (atHands [0])
        [ voiceStack (atChords [0])    (setOctave 5 . setDegree 1) [-1, -2]
        , voiceStack (atChords [1, 2]) (setOctave 4 . setDegree 2) [-1]
        , Leaf (atChords [3] . atVoices [0]) (setOctave 5 . setDegree 0)
        ]
        -- Left hand
      , Node
        (atHands [1])
        [ Leaf (atChords [0]) (setOctave 2 . setDegree 0)
        , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-1, -2]
        , voiceStack (atChords [2]) (setOctave 3 . setDegree 1) [-1]
        ]
      ]
      -- Measures 0, 1
    , Node
      (atMeasures [0, 1])
      [ Leaf id (setScale (mkMajorScale 8))
          -- Right hand
      , Node
        (atHands [0] . atChords [0, 1, 2, 3])
        [ voiceStack
          (atChords [0])
          (modifyScale (extractTriad 5) . setOctave 5 . setDegree 0)
          [-1, -2]
        , Leaf (atChords [1, 2, 3]) (modifyScale (extractTriad 0))
        , voiceStack (atChords [1, 2] . atVoices [0, 1])
                     (setOctave 4 . setDegree 1)
                     [-1]
        , Leaf (atChords [3] . atVoices [0]) (setOctave 4 . setDegree 2)
        ]
        -- Left hand
      , Node
        (atHands [1] . atChords [0, 1, 2])
        [ Node
          (atChords [0, 1])
          [ Leaf id                            (modifyScale (extractTriad 5))
          , Leaf (atChords [0] . atVoices [0]) (setOctave 2 . setDegree 0)
          , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-1, -2, -3]
          ]
        , voiceStack
          (atChords [2])
          (modifyScale (extractTriad 0) . setOctave 3 . setDegree 1)
          [-2, -4]
        ]
      ]
    ]
  ]

phraseBMeasure2Period2 :: MusicTree
phraseBMeasure2Period2 = Node
  (atPhrases [1] . atMeasures [2])
  [ Leaf (atChords [0])    (setScale (extractTriad 0 (mkMajorScale 8)))
  , Leaf (atChords [1, 2]) (setScale (extractSeventh 4 (mkMajorScale 8)))
  , Node
    (atHands [0])
    [ evenRhythmMeterR
    , voiceStack (atChords [0]) (setOctave 5 . setDegree 0) [-3]
    , Node
      (atChords [0] . atVoices [0])
      [ Leaf (atNotes [0, 1]) (setDuration (1 / 8))
      , Leaf (atNotes [1])    (setVolume 90 . modifyAbsDegree (subtract 1))
      ]
    , voiceStack (atChords [1]) (setOctave 4 . setDegree 3) [-2]
    , voiceStack (atChords [2]) (setOctave 4 . setDegree 2) [-2, -3]
    ]
  , Node
    (atHands [1])
    [ evenRhythmMeterL
    , Leaf (atChords [0]) (setOctave 1 . setDegree 1)
    , Leaf (atChords [1]) (setOctave 2 . setDegree 0)
    , voiceStack (atChords [2]) (setOctave 3 . setDegree 1) [-1]
    ]
  ]

phraseBPeriods012 :: MusicTree
phraseBPeriods012 = Node
  (atPeriods [0, 1, 2] . atPhrases [1] . atMeasures [0, 1, 2, 3])
  [ Leaf id (setScale (mkMajorScale 8))
    -- Measure 2
  , Node
    (atMeasures [2])
    [ Node (atPeriods [0, 1]) [phraseBMeasure2Periods01]
    , Node (atPeriods [2])    [phraseBMeasure2Period2]
    ]
    -- Measures 0, 1, 3
  , Node
    (atMeasures [0, 1, 3])
    [ -- Base rhythm and meter
      Node (atHands [0]) [unevenRhythmMeter]
    , Node (atHands [1]) [evenRhythmMeterL]
      -- Measures 0, 1
    , Node
      (atMeasures [0, 1])
      [ -- Right hand
        Node
        (atHands [0] . atChords [0, 1, 2, 3])
        [ voiceStack
          (atChords [0])
          (modifyScale (extractTriad 5) . setOctave 5 . setDegree 0)
          [-1, -2]
        , Leaf (atChords [1, 2, 3]) (modifyScale (extractTriad 0))
        , voiceStack (atChords [1, 2] . atVoices [0, 1])
                     (setOctave 4 . setDegree 1)
                     [-1]
        , Leaf (atChords [3] . atVoices [0]) (setOctave 4 . setDegree 2)
        ]
        -- Left hand
      , Node
        (atHands [1] . atChords [0, 1, 2])
        [ Node
          (atChords [0, 1])
          [ Leaf id                            (modifyScale (extractTriad 5))
          , Leaf (atChords [0] . atVoices [0]) (setOctave 2 . setDegree 0)
          , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-1, -2, -3]
          ]
        , voiceStack
          (atChords [2])
          (modifyScale (extractTriad 0) . setOctave 3 . setDegree 1)
          [-2, -4]
        ]
      ]
      -- Measure 3
    , Node
      (atMeasures [3])
      [ -- Periods 0, 1
        Node
        (atPeriods [0, 1])
        [ Leaf id (modifyScale (extractTriad 2))
        , Node
          (atHands [0])
          [ voiceStack (atChords [0]) (setOctave 5 . setDegree 1) [-1, -2]
          , Node
            (atPeriods [0])
            [ voiceStack (atChords [1, 2]) (setOctave 4 . setDegree 2) [-1]
            , Leaf (atChords [3] . atVoices [0]) (setOctave 5 . setDegree 0)
            ]
          , Node
            (atPeriods [1])
            [ voiceStack (atChords [1, 2]) (setOctave 5 . setDegree 0) [-1, -2]
            , Leaf (atChords [3] . atVoices [0]) (setOctave 5 . setDegree 1)
            ]
          ]
        -- Left hand
        , Node
          (atHands [1])
          [ Leaf (atChords [0]) (setOctave 2 . setDegree 0)
          , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-1, -2]
          , voiceStack (atPeriods [0] . atChords [2])
                       (setOctave 3 . setDegree 1)
                       [-1]
          , voiceStack (atPeriods [1, 2] . atChords [2])
                       (setOctave 3 . setDegree 2)
                       [-1, -2]
          ]
        ]
        -- Period 2
      , Node
        (atPeriods [2])
        [ Leaf id (modifyScale (extractTriad 0))
        , Node
          (atHands [0])
          [ Leaf id (setVolume 100)
          , voiceStack (atChords [0])
                       (setOctave 4 . setDegree 0 . setDuration (1 / 4))
                       [-1, -2]
          , Leaf (atChords [1]) (setVolume 0 . setDuration (1 / 8))
          , voiceStack (atChords [2, 3])
                       (setOctave 4 . setDegree 1 . setDuration (1 / 8))
                       [-1, -2]
          , Leaf (atChords [4])
                 (setOctave 4 . setDegree 2 . setDuration (1 / 8))
          ]
        , Node
          (atHands [1])
          [ evenRhythmMeterL
          , Leaf (atChords [0]) (setOctave 2 . setDegree 0)
          , voiceStack (atChords [1]) (setOctave 3 . setDegree 0) [-1]
          , voiceStack (atChords [2]) (setOctave 3 . setDegree 1) [-1, -2]
          ]
        ]
      ]
    ]
  ]

phraseBPeriods0123 :: MusicTree
phraseBPeriods0123 = Node
  id
  [ Node (atPeriods [0, 1, 2]) [phraseBPeriods012]
  , Node
    (atPeriods [3])
    [ Node
      (atMeasures [0, 1])
      [ Node
        (atHands [0])
        [ voiceStack
          (atChords [0])
          ( setVolume 100
          . setOctave 6
          . setDegree 0
          . setDuration (1 / 4)
          . setScale (extractTriad 5 $ mkMajorScale 8)
          )
          [-2]
        , voiceStack
          (atChords [1, 2, 3, 4, 5, 6])
          ( setScale (extractTriad 0 $ mkMajorScale 8)
          . setDuration (1 / 12)
          . setVolume 80
          . setOctave 4
          . setDegree 0
          )
          [-2]
        , Leaf (atChords [1])          (setVolume 0)
        , Leaf (atChords [2])          id
        , Leaf (atChords [3, 4, 5, 6]) (modifyAbsDegree (+ 1))
        , Leaf (atChords [4, 5, 6])    (modifyAbsDegree (+ 2))
        , Leaf (atChords [5, 6])       (modifyAbsDegree (+ 1))
        , Leaf (atChords [6])
               (modifyScalePitch (roundUp (extractTriad 5 $ mkMajorScale 8)))
        ]
      , Node
        (atHands [1])
        [ Leaf id (setDuration (1 / 4) . setVolume 50)
        , Leaf
          (atChords [0])
          (setScale (extractTriad 5 (mkMajorScale 8)) . setOctave 2 . setDegree
            0
          )
        , voiceStack
          (atChords [1])
          (setScale (extractTriad 5 (mkMajorScale 8)) . setOctave 3 . setDegree
            2
          )
          [-1, -2]
        , voiceStack
          (atChords [2])
          (setScale (extractTriad 0 (mkMajorScale 8)) . setOctave 3 . setDegree
            0
          )
          [-1, -3]
        ]
      ]
    , Node
      (atMeasures [2])
      [ Leaf (atChords [0])    (setScale (extractTriad 0 (mkMajorScale 8)))
      , Leaf (atChords [1, 2]) (setScale (extractSeventh 4 (mkMajorScale 8)))
      , Node
        (atHands [0])
        [ Leaf id (setVolume 100 . setDuration (1 / 4))
        , Node
          (atChords [0] . atNotes [0, 1])
          [ Leaf id            (setDuration (1 / 8) . setOctave 6 . setDegree 0)
          , Leaf (atNotes [1]) (modifyAbsDegree (subtract 1))
          ]
        , voiceStack (atChords [1, 2]) (setOctave 5 . setDegree 3) [-2, -3]
        , Leaf (atChords [2]) (modifyAbsDegree (subtract 1))
        ]
      , Node
        (atHands [1])
        [ evenRhythmMeterL
        , Leaf (atChords [0]) (setOctave 1 . setDegree 1)
        , Leaf (atChords [1]) (setOctave 2 . setDegree 0)
        , voiceStack (atChords [2]) (setOctave 3 . setDegree 3) [-2, -3]
        ]
      ]
    , Node
      (atMeasures [3])
      [ voiceStack
        (atHands [0])
        ( setScale (extractTriad 0 (mkMajorScale 8))
        . setOctave 5
        . setDegree 0
        . setVolume 100
        . setDuration (3 / 4)
        )
        [-2, -3]
      , Node
        (atHands [1])
        [ Leaf
          id
          ( setScale (extractTriad 0 (mkMajorScale 8))
          . setVolume 50
          . setDuration (1 / 4)
          )
        , Leaf (atChords [0]) (setOctave 1 . setDegree 0)
        , voiceStack (atChords [1]) (setOctave 3 . setDegree 1) [-1, -2]
        ]
      ]
    ]
  ]

