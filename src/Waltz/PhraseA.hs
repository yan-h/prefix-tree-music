module Waltz.PhraseA where

import           Event
import           Tree
import           Waltz.Shared

-- Measure 0, right hand only
phraseAHand0Measure0 :: MusicTree
phraseAHand0Measure0 = Node
  (atChords [0, 1, 2, 3] . atVoices [0, 1])
  [ Leaf
    id
    ( setDuration (3 / 8)
    . setVolume 120
    . setScale (extractTriad 0 $ mkMajorScale 8)
    . setOctave 4
    . setDegree 1
    )
  , Leaf (atChords [1, 2, 3]) (setDuration (1 / 8) . modifyVolume (subtract 20))
  , Leaf (atChords [1, 2])    (modifyAbsDegree (subtract 1))
  , Leaf (atVoices [1])
         (modifyAbsDegree (subtract 2) . modifyVolume (subtract 20))
  ]

-- Both hands, Measures 0, 1.
phraseAMeasures01 :: MusicTree
phraseAMeasures01 = Node
  (atMeasures [0, 1])
  [ -- Everything has the same scale
    Leaf id (setScale (extractTriad 0 (mkMajorScale 8)))
  , Node
    (atHands [0])
    [ Leaf id                   (setOctave 4 . setDegree 1)
    , Leaf (atChords [0])       (setDuration (3 / 8) . setVolume 100)
    , Leaf (atChords [1, 2, 3]) (setDuration (1 / 8) . setVolume 80)
    , Leaf (atChords [1, 2])    (modifyAbsDegree (subtract 1))
    , Leaf (atVoices [1])       (modifyAbsDegree (subtract 2))
    ]
  , Node
    (atHands [1])
    [ Leaf id (setDuration (1 / 4) . setVolume 60)
    , Node -- A leaf was split into this node
      (atChords [0])
      [ Leaf (atMeasures [0]) (setOctave 2 . setDegree 0)
      , Leaf (atMeasures [1]) (setOctave 1 . setDegree 0)
      ]
    , voiceStack (atChords [1, 2]) (setOctave 3 . setDegree 1) [-1, -2]
    , Leaf (atChords [2]) (modifyAbsDegree (subtract 1))
    ]
  ]

-- Measure 2, using voice leading functions for the right hand.
phraseAMeasure2 :: MusicTree
phraseAMeasure2 = Node
  (atMeasures [2])
  [ Leaf id (setScale (mkMajorScale 8) . setDuration (1 / 4) . setVolume 100)
  , Node
    (atHands [0])
    [ Leaf id (setDuration (1 / 4) . setVolume 100)
    , voiceStack
      (atChords [0, 1, 2])
      (setScale (extractTriad 3 (mkMajorScale 8)) . setOctave 5 . setDegree 0)
      [-1, -2]
    , Node
      (atChords [0] . atVoices [0])
      [ Leaf id (setDuration (1 / 8))
      , Leaf
        (atNotes [1, 2])
        (setDuration (1 / 16) . modifyScalePitch (stepUp (mkMajorScale 8)))
      , Leaf (atNotes [2]) (modifyScalePitch (stepDown (mkMajorScale 8)))
      ]
    , Leaf (atChords [1, 2])
           (modifyScalePitch (roundDown (extractTriad 0 $ mkMajorScale 8)))
    , Leaf (atChords [2])
           (modifyScalePitch (roundDown (extractSeventh 1 $ mkMajorScale 8)))
    ]
  , Node
    (atHands [1])
    [ Leaf id                (setDuration (1 / 4) . setVolume 60)
    , Leaf (atChords [1, 2]) (modifyScale (extractTriad 3))
    , Leaf (atChords [0])    (setOctave 2 . setDegree 0)
    , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-2, -3]
    , voiceStack (atChords [2]) (setOctave 3 . setDegree 2) [-1]
    ]
  ]

-- The complete first phrase, identical across periods 0, 1, and 2.
-- Measure 2 is pasted; measures 0 and 1 were reworked 
-- so some material could be shared with measure 3.
phraseAPeriods012 :: MusicTree
phraseAPeriods012 = Node
  (atMeasures [0, 1, 2, 3] . atHands [0, 1])
  [ -- Measure 2
    Node
    (atMeasures [2])
    [ Leaf id (setScale (mkMajorScale 8) . setDuration (1 / 4) . setVolume 100)
    , Node
      (atHands [0])
      [ Leaf id (setDuration (1 / 4) . setVolume 100)
      , voiceStack
        (atChords [0, 1, 2])
        (setScale (extractTriad 3 (mkMajorScale 8)) . setOctave 5 . setDegree 0)
        [-1, -2]
      , Node
        (atChords [0] . atVoices [0])
        [ Leaf id (setDuration (1 / 8))
        , Leaf
          (atNotes [1, 2])
          (setDuration (1 / 16) . modifyScalePitch (stepUp (mkMajorScale 8)))
        , Leaf (atNotes [2]) (modifyScalePitch (stepDown (mkMajorScale 8)))
        ]
      , Leaf (atChords [1, 2])
             (modifyScalePitch (roundDown (extractTriad 0 $ mkMajorScale 8)))
      , Leaf
        (atChords [2])
        (modifyScalePitch (roundDown (extractSeventh 1 $ mkMajorScale 8)))
      ]
    , Node
      (atHands [1])
      [ Leaf id                (setDuration (1 / 4) . setVolume 60)
      , Leaf (atChords [1, 2]) (modifyScale (extractTriad 3))
      , Leaf (atChords [0])    (setOctave 2 . setDegree 0)
      , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-2, -3]
      , voiceStack (atChords [2]) (setOctave 3 . setDegree 2) [-1]
      ]
    ]
    -- Measures 0, 1, 3
  , Node
    (atMeasures [0, 1, 3])
    [ -- Common scale
      Leaf id (setScale (extractTriad 0 (mkMajorScale 8)))
      -- Right hand
    , Node
      (atHands [0] . atChords [0, 1, 2, 3])
      [ -- Common rhythm
        unevenRhythmMeter
        -- Common root pitch
      , Leaf id (setOctave 4 . setDegree 1)
        -- Measures 0, 1
      , Node
        (atMeasures [0, 1] . atVoices [0, 1])
        [ Leaf (atChords [1, 2]) (modifyAbsDegree (subtract 1))
        , Leaf (atVoices [1])    (modifyAbsDegree (subtract 2))
        ]
        -- Measure 3
      , Node
        (atMeasures [3])
        [ voiceStack (atChords [0]) id [-1, -2]
        , Leaf (atChords [1, 2] . atVoices [0]) (modifyAbsDegree (subtract 1))
        , voiceStack (atChords [3]) (modifyAbsDegree (+ 1)) [-1]
        ]
      ]
      -- Left hand
    , Node
      (atHands [1] . atChords [0, 1, 2])
      [ -- Common rhythm and meter
        evenRhythmMeterL
        -- Chord 0
      , Node
        (atChords [0] . atVoices [0])
        [ Leaf (atMeasures [0])    (setOctave 2 . setDegree 0)
        , Leaf (atMeasures [1, 3]) (setOctave 1 . setDegree 0)
        ]
        -- Chords 1, 2
      , Node
        (atChords [1, 2])
        [ voiceStack id (setOctave 3 . setDegree 1) [-1, -2]
        , Leaf (atChords [2]) (modifyAbsDegree (subtract 1))
        ]
      ]
    ]
  ]

-- The complete first phrase, including the variation in period 3.
phraseAPeriods0123 :: MusicTree
phraseAPeriods0123 = Node
  (atPeriods [0, 1, 2, 3] . atMeasures [0, 1, 2, 3] . atHands [0, 1])
  [ -- Measure 2
    Node
    (atMeasures [2])
    [ Leaf id (setScale (mkMajorScale 8) . setDuration (1 / 4) . setVolume 100)
    , Node
      (atHands [0])
      [ Leaf id (setDuration (1 / 4) . setVolume 100)
      , voiceStack
        (atChords [0, 1, 2])
        (setScale (extractTriad 3 (mkMajorScale 8)) . setOctave 5 . setDegree 0)
        [-1, -2]
      , Node
        (atChords [0] . atVoices [0])
        [ Leaf id (setDuration (1 / 8))
        , Leaf
          (atNotes [1, 2])
          (setDuration (1 / 16) . modifyScalePitch (stepUp (mkMajorScale 8)))
        , Leaf (atNotes [2]) (modifyScalePitch (stepDown (mkMajorScale 8)))
        ]
      , Leaf (atChords [1, 2])
             (modifyScalePitch (roundDown (extractTriad 0 $ mkMajorScale 8)))
      , Leaf
        (atChords [2])
        (modifyScalePitch (roundDown (extractSeventh 1 $ mkMajorScale 8)))
      , Leaf (atPeriods [3]) (modifyOctave (+ 1))
      ]
    , Node
      (atHands [1])
      [ Leaf id                (setDuration (1 / 4) . setVolume 60)
      , Leaf (atChords [1, 2]) (modifyScale (extractTriad 3))
      , Leaf (atChords [0])    (setOctave 2 . setDegree 0)
      , voiceStack (atChords [1]) (setOctave 4 . setDegree 0) [-2, -3]
      , voiceStack (atChords [2]) (setOctave 3 . setDegree 2) [-1]
      ]
    ]
    -- Measures 0, 1, 3
  , Node
    (atMeasures [0, 1, 3])
    [ -- Common scale
      Leaf id (setScale (extractTriad 0 (mkMajorScale 8)))
      -- Right hand
    , Node
      (atHands [0] . atChords [0, 1, 2, 3])
      [ -- Common rhythm
        Node (atPeriods [0, 1, 2]) [unevenRhythmMeter]
        -- Common root pitch
      , Leaf id (setOctave 4 . setDegree 1)
        -- Measures 0, 1
      , Node
        (atPeriods [0, 1, 2])
        [ Node
          (atMeasures [0, 1] . atVoices [0, 1])
          [ Leaf (atChords [1, 2]) (modifyAbsDegree (subtract 1))
          , Leaf (atVoices [1])    (modifyAbsDegree (subtract 2))
          ]
        -- Measure 3
        , Node
          (atMeasures [3])
          [ voiceStack (atChords [0]) id [-1, -2]
          , Leaf (atChords [1, 2] . atVoices [0]) (modifyAbsDegree (subtract 1))
          , voiceStack (atChords [3]) (modifyAbsDegree (+ 1)) [-1]
          ]
        ]
      , Node
        (atPeriods [3] . atChords [0, 1, 2, 3, 4, 5, 6])
        [ Node
          (atChords [0])
          [ Leaf
            id
            (setDuration (1 / 4) . setVolume 100 . setOctave 5 . setDegree 1)
          , voiceStack (atMeasures [0, 1]) id [-2]
          , voiceStack (atMeasures [3])    id [-1, -2]
          ]
        , voiceStack
          (atChords [1, 2, 3, 4, 5, 6])
          (setDuration (1 / 12) . setVolume 80 . setOctave 4 . setDegree 0)
          [-2]
        , Leaf (atChords [1])          (setVolume 0)
        , Leaf (atChords [2])          id
        , Leaf (atChords [3, 4, 5, 6]) (modifyAbsDegree (+ 1))
        , Leaf (atChords [4, 5, 6])    (modifyAbsDegree (+ 2))
        , Leaf (atChords [5])          (modifyAbsDegree (+ 1))
        , Leaf
          (atChords [6] . atMeasures [3])
          ( modifyScalePitch (roundUp (extractTriad 5 (mkMajorScale 8)))
          . modifyAbsDegree (+ 1)
          )
        ]
      ]
      -- Left hand
    , Node
      (atHands [1] . atChords [0, 1, 2])
      [ -- Common rhythm and meter
        evenRhythmMeterL
        -- Chord 0
      , Node
        (atChords [0] . atVoices [0])
        [ Leaf id (setOctave 1 . setDegree 0)
        , Leaf (atPeriods [0, 1, 2] . atMeasures [0]) (modifyOctave (+ 1))
        ]
        -- Chords 1, 2
      , Node
        (atChords [1, 2])
        [ voiceStack id (setOctave 3 . setDegree 1) [-1, -2]
        , Leaf (atChords [2]) (modifyAbsDegree (subtract 1))
        ]
      ]
    ]
  ]
