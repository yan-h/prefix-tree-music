module Waltz.Shared where

import           Event
import           Tree

voiceStack :: (Slice -> Slice) -> (Event -> Event) -> [Int] -> MusicTree
voiceStack location base offsets =
  Node location $ Leaf (atVoices [0 .. length offsets]) base : zipWith
    (\offset voiceIdx -> Leaf (atVoices [voiceIdx]) (modifyAbsDegree (+ offset))
    )
    offsets
    [1 ..]

evenRhythmMeterL :: MusicTree
evenRhythmMeterL =
  Leaf (atChords [0, 1, 2]) (setDuration (1 / 4) . setVolume 60)

evenRhythmMeterR :: MusicTree
evenRhythmMeterR =
  Leaf (atChords [0, 1, 2]) (setDuration (1 / 4) . setVolume 100)

unevenRhythmMeter :: MusicTree
unevenRhythmMeter = Node
  (atHands [0])
  [ Leaf (atChords [0])       (setDuration (3 / 8) . setVolume 100)
  , Leaf (atChords [1, 2, 3]) (setDuration (1 / 8) . setVolume 90)
  ]