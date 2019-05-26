{-# LANGUAGE TemplateHaskell,  FlexibleInstances, FunctionalDependencies #-}

module Event where

import           Control.Lens
import           Euterpea

import           Data.List                      ( nub
                                                , sort
                                                , minimumBy
                                                , elemIndex
                                                )

import           Data.Ord                       ( comparing )

import           Data.Maybe                     ( fromJust )

type Degree = Int

--------------------------------------------------------------------------------
-- PC
--------------------------------------------------------------------------------

-- | Represents a pitch class; an 'AbsPitch' mod 12.
newtype PC = PC { _int :: AbsPitch }
  deriving (Eq, Ord, Show)

instance Num PC where
  (PC a) + (PC b) = mkPC $ a + b
  (PC a) - (PC b) = mkPC $ a - b
  (PC a) * (PC b) = mkPC $ a * b
  abs         = PC . abs . _int
  negate      = PC . negate . _int
  signum      = PC . signum . _int
  fromInteger = mkPC . fromInteger

-- | Creates a 'PC' from an 'AbsPitch' by modding by 12.
mkPC :: AbsPitch -> PC
mkPC = PC . (`mod` 12)

--------------------------------------------------------------------------------
-- Scale
--------------------------------------------------------------------------------

-- | A scale, defined by a root pitch class and a set of pitch class offsets from it. 
data Scale = Scale
  { _root :: PC
  , _offsets :: [PC]
  }
  deriving (Eq, Ord, Show)

class GetSize a where
  getSize :: a -> Int

instance GetSize Scale where
  getSize (Scale _ offsets) = 1 + length offsets

mkPCList :: Scale -> [PC]
mkPCList (Scale root offsets) = (+ root) <$> (0 : offsets)

mkScale :: PC -> [PC] -> Scale
mkScale root offsets =
  let cleanOffsets = nub . sort . filter (/= 0) $ offsets
  in  Scale root cleanOffsets

-- Extracts a triad from a scale, starting on the input scale degree
extractTriad :: Int -> Scale -> Scale
extractTriad root scale =
  let convert idx = mkPCList scale !! (idx `mod` getSize scale)
      cRoot = convert root
  in  mkScale cRoot [convert (root + 2) - cRoot, convert (root + 4) - cRoot]

-- Extracts a seventh from a scale, starting on the input scale degree
extractSeventh :: Int -> Scale -> Scale
extractSeventh root scale =
  let convert idx = mkPCList scale !! (idx `mod` getSize scale)
      cRoot = convert root
  in  mkScale
        cRoot
        [ convert (root + 2) - cRoot
        , convert (root + 4) - cRoot
        , convert (root + 6) - cRoot
        ]

mkMajorScale :: PC -> Scale
mkMajorScale root = mkScale root [2, 4, 5, 7, 9, 11]

mkMinorScale :: PC -> Scale
mkMinorScale root = mkScale root [2, 3, 5, 7, 8, 10]

mkMajorTriad :: PC -> Scale
mkMajorTriad root = mkScale root [4, 7]

mkMinorTriad :: PC -> Scale
mkMinorTriad root = mkScale root [3, 7]

--------------------------------------------------------------------------------
-- ScalePitch
--------------------------------------------------------------------------------

-- | A pitch on a scale. Determined by a scale, octave and scale degree number.
data ScalePitch = ScalePitch
  { _scale :: Scale
  , _octave :: Octave
  , _degree :: Degree
  }
  deriving (Show)

makeLenses ''ScalePitch

instance GetSize ScalePitch where
  getSize (ScalePitch scale octave degree) = getSize scale

getPitch :: ScalePitch -> AbsPitch
getPitch (ScalePitch scale octave degree) =
  let pitchOffset = case degree of
        0 -> _int . _root $ scale
        _ -> (_int . _root) scale + _int (_offsets scale !! (degree - 1))
  in  12 * (octave + 1) + pitchOffset

mkScalePitch :: Scale -> Octave -> Degree -> ScalePitch
mkScalePitch scale octave degree =
  ScalePitch scale octave (degree `mod` getSize scale)

absDegree :: Lens' ScalePitch Int
absDegree = lens get set
 where
  get s = _octave s * getSize s + _degree s
  set s newAbsDegree = s { _octave = newAbsDegree `div` getSize s
                         , _degree = newAbsDegree `mod` getSize s
                         }

-- | Given a pitch, returns the lowest ScalePitch in a given scale that is higher.
stepUp :: Scale -> ScalePitch -> ScalePitch
stepUp scale original =
  let refPitch   = getPitch original
      refPC    = mkPC refPitch
      scalePCs = mkPCList scale

      finalPC  = minimumBy (comparing (subtract 1 . subtract refPC)) scalePCs
      finalIdx = fromJust $ elemIndex finalPC scalePCs

      candidates :: [ScalePitch]
      candidates = fmap
        ($ mkScalePitch scale (refPitch `div` 12) finalIdx)
        [ over octave (+ 1)
        , id
        , over octave (subtract 1)
        , over octave (subtract 2)
        ]
  in  minimumBy (comparing (\sp -> abs (getPitch sp - refPitch)))
        . filter ((> refPitch) . getPitch)
        $ candidates

-- | Given a pitch, returns the highest ScalePitch in a given scale that is lower. 
stepDown :: Scale -> ScalePitch -> ScalePitch
stepDown scale original =
  let
    refPitch   = getPitch original
    refPC    = mkPC refPitch
    scalePCs = mkPCList scale

    finalPC  = minimumBy (comparing (subtract 1 . (`subtract` refPC))) scalePCs
    finalIdx = fromJust $ elemIndex finalPC scalePCs

    candidates :: [ScalePitch]
    candidates = fmap
      ($ mkScalePitch scale (refPitch `div` 12) finalIdx)
      [ over octave (+ 1)
      , id
      , over octave (subtract 1)
      , over octave (subtract 2)
      ]
  in
    minimumBy (comparing (\sp -> abs (getPitch sp - refPitch)))
    . filter ((< refPitch) . getPitch)
    $ candidates


-- | Given a pitch, returns the lowest 'ScalePitch' in a given scale that is
--   equal or higher.
roundUp :: Scale -> ScalePitch -> ScalePitch
roundUp scale original =
  let refPitch   = getPitch original
      refPC    = mkPC refPitch
      scalePCs = mkPCList scale

      finalPC  = minimumBy (comparing (subtract refPC)) scalePCs
      finalIdx = fromJust $ elemIndex finalPC scalePCs

      candidates :: [ScalePitch]
      candidates = fmap
        ($ mkScalePitch scale (refPitch `div` 12) finalIdx)
        [ over octave (+ 1)
        , id
        , over octave (subtract 1)
        , over octave (subtract 2)
        ]
  in  minimumBy (comparing (\sp -> abs (getPitch sp - refPitch)))
        . filter ((>= refPitch) . getPitch)
        $ candidates

-- | Given a pitch, returns the highest 'ScalePitch' in a given scale that is 
--   equal or lower.
roundDown :: Scale -> ScalePitch -> ScalePitch
roundDown scale original =
  let refPitch   = getPitch original
      refPC    = mkPC refPitch
      scalePCs = mkPCList scale

      finalPC  = minimumBy (comparing (`subtract` refPC)) scalePCs
      finalIdx = fromJust $ elemIndex finalPC scalePCs

      candidates :: [ScalePitch]
      candidates = fmap
        ($ mkScalePitch scale (refPitch `div` 12) finalIdx)
        [ over octave (+ 1)
        , id
        , over octave (subtract 1)
        , over octave (subtract 2)
        ]
  in  minimumBy (comparing (\sp -> abs (getPitch sp - refPitch)))
        . filter ((<= refPitch) . getPitch)
        $ candidates

lead :: (Int -> Int) -> Scale -> ScalePitch -> ScalePitch
lead f newScale scalePitch
  | newScale == scalePitch ^. scale
  = scalePitch & absDegree %~ f
  | absDegreeDiff == 0
  = roundDown newScale scalePitch
  | absDegreeDiff > 0
  = stepUp newScale scalePitch & absDegree %~ (+ (absDegreeDiff - 1))
  | otherwise
  = stepDown newScale scalePitch
    &  absDegree
    %~ (+ (absDegreeDiff + 1))
 where
  curAbsDegree  = scalePitch ^. absDegree
  newAbsDegree  = f $ scalePitch ^. absDegree
  absDegreeDiff = newAbsDegree - curAbsDegree
--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------

data Event = Event
  { _duration :: Dur
  , _volume :: Volume
  , _scalePitch :: ScalePitch
  }
  deriving (Show)

makeLenses ''Event

defaultEvent :: Event
defaultEvent = Event 0 0 (ScalePitch (mkMajorScale 0) 0 0)

toPitch :: Event -> AbsPitch
toPitch = getPitch . view scalePitch

toPrimitive :: Event -> Primitive (AbsPitch, Volume)
toPrimitive (Event dur vol (ScalePitch scale octave degree))
  | vol == 0
  = Rest dur
  | otherwise
  = let absPitch = getPitch $ mkScalePitch scale octave degree
    in  Note dur (absPitch, vol)

modifyDuration :: (Dur -> Dur) -> Event -> Event
modifyDuration = over duration

modifyVolume :: (Int -> Int) -> Event -> Event
modifyVolume = over volume

modifyScale :: (Scale -> Scale) -> Event -> Event
modifyScale = over (scalePitch . scale)

modifyOctave :: (Int -> Int) -> Event -> Event
modifyOctave = over (scalePitch . octave)

modifyDegree :: (Int -> Int) -> Event -> Event
modifyDegree = over (scalePitch . degree)

modifyScalePitch :: (ScalePitch -> ScalePitch) -> Event -> Event
modifyScalePitch = over scalePitch

modifyAbsDegree :: (Int -> Int) -> Event -> Event
modifyAbsDegree = over (scalePitch . absDegree)

setDuration :: Dur -> Event -> Event
setDuration = modifyDuration . const

setVolume :: Int -> Event -> Event
setVolume = modifyVolume . const

setScale :: Scale -> Event -> Event
setScale = modifyScale . const

setOctave :: Octave -> Event -> Event
setOctave = modifyOctave . const

setDegree :: Int -> Event -> Event
setDegree = modifyDegree . const

setScalePitch :: ScalePitch -> Event -> Event
setScalePitch = modifyScalePitch . const
