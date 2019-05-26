module Waltz.PhraseA where

import           Event
import           Tree
import           Waltz.Shared

-- Measure 0, right hand only
phraseAMeasure0RH :: PrefixTree (Slice -> Slice) (Event -> Event)
phraseAMeasure0RH = Node
  (atChords [0, 1, 2, 3] . atVoices [0, 1])
  [ Leaf
    id
    ( setDuration (3 / 8)
    . setVolume 100
    . setScale (extractTriad 0 $ mkMajorScale 8)
    . setOctave 4
    . setDegree 1
    )
  , Leaf (atChords [1, 2, 3]) (setDuration (1 / 8) . modifyVolume (subtract 20))
  , Leaf (atChords [1, 2])    (modifyAbsDegree (subtract 1))
  , Leaf (atVoices [1])
         (modifyAbsDegree (subtract 2) . modifyVolume (subtract 20))
  ]

-- Measure 0, both hands.
-- To save space, we paste 'phraseAMeasure0RH' in instead of repeating the right hand
phraseAMeasure0 :: PrefixTree (Slice -> Slice) (Event -> Event)
phraseAMeasure0 = Node
  id
  [ Leaf id (setScale (mkMajorScale 8))
  , Node (atHands [0]) [phraseAMeasure0RH]
  , Node
    (atHands [1])
    [ Leaf id
           (modifyScale (extractTriad 0) . setDuration (1 / 4) . setVolume 50)
    , Leaf (atChords [0] . atVoices [0]) (setOctave 2 . setDegree 0)
    , voiceStack (atChords [1, 2]) (setOctave 3 . setDegree 1) [-1, -2]
    , Leaf (atChords [2]) (modifyAbsDegree (subtract 1))
    ]
  ]

-- Measures 0, 1.
-- The only difference is the first chord in the left hand. We implement this
-- by splitting its leaf into two branches, one for measure 0 and one for measure 1.
phraseAFirstTwoMeasures :: PrefixTree (Slice -> Slice) (Event -> Event)
phraseAFirstTwoMeasures = Node
  (atMeasures [0, 1])
  [ Leaf id (setScale (mkMajorScale 8))
  , Node (atHands [0]) [phraseAMeasure0RH]
  , Node
    (atHands [1])
    [ Leaf id
           (modifyScale (extractTriad 0) . setDuration (1 / 4) . setVolume 50)
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
    [ voiceStack (atChords [0, 1, 2])
                 (modifyScale (extractTriad 3) . setOctave 5 . setDegree 0)
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
    [ Leaf id             (setDuration (1 / 4) . setVolume 50)
    , Leaf (atVoices [0]) (setVolume 90)
      -- Chord 0
    , Leaf (atChords [0]) (setOctave 2 . setDegree 0)
      -- Chord 1
    , voiceStack (atChords [1])
                 (modifyScale (extractTriad 3) . setOctave 4 . setDegree 0)
                 [-2, -3]
      -- Chord 2
    , voiceStack (atChords [2])
                 (modifyScale (extractTriad 3) . setOctave 3 . setDegree 2)
                 [-1]
    ]
  ]

-- The complete first phrase in period 0.
-- The third measure is pasted; the first and second measures were reworked 
-- so some material could be shared with the fourth.
phraseA :: MusicTree
phraseA = Node
  (atMeasures [0, 1, 2, 3] . atHands [0, 1])
  [ phraseAMeasure2
  , Node
    (atMeasures [0, 1, 3])
    [ Leaf id (setScale (mkMajorScale 8))
    , Node
      (atHands [0] . atChords [0, 1, 2, 3])
      [ rhRhythmMeter
      , Leaf
        id
        (setScale (extractTriad 0 $ mkMajorScale 8) . setOctave 4 . setDegree 1)
      , Leaf (atChords [1, 2, 3]) (setDuration (1 / 8))
      , Node
        (atMeasures [0, 1] . atVoices [0, 1])
        [ Leaf (atChords [1, 2]) (modifyAbsDegree (subtract 1))
        , Leaf (atVoices [1])    (modifyAbsDegree (subtract 2))
        ]
      , Node
        (atMeasures [3])
        [ voiceStack (atChords [0]) (setOctave 4 . setDegree 1) [-1, -2]
        , Leaf (atChords [1, 2] . atVoices [0]) (modifyAbsDegree (subtract 1))
        , Node
          (atChords [3])
          [Leaf (atVoices [0]) (modifyAbsDegree (+ 1)), Leaf (atVoices [1]) id]
        ]
      ]
    , Node
      (atHands [1])
      [ lhRhythmMeter
      , Leaf id (modifyScale (extractTriad 0))
      , Node
        (atChords [0] . atVoices [0])
        [ Leaf (atMeasures [0])    (setOctave 2 . setDegree 0)
        , Leaf (atMeasures [1, 3]) (setOctave 1 . setDegree 0)
        ]
      , Node
        (atChords [1, 2])
        [ voiceStack id (setOctave 3 . setDegree 1) [-1, -2]
        , Leaf (atChords [2]) (modifyAbsDegree (subtract 1))
        ]
      ]
    ]
  ]
