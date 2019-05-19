{-# LANGUAGE TemplateHaskell,  FlexibleInstances, FunctionalDependencies #-}

module ScalePitch where

import           Data.List                      ( nub
                                                , sort
                                                , minimumBy
                                                )
import           Euterpea
import           Control.Lens

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

-- | Creates a `PC` from a `Pitch` by modding by 12.
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

getPCList :: Scale -> [PC]
getPCList (Scale root offsets) = (+ root) <$> (0 : offsets)

mkScale :: PC -> [PC] -> Scale
mkScale root offsets =
  let cleanOffsets = nub . sort . filter (/= 0) $ offsets
  in  Scale root cleanOffsets

extractTriad :: Int -> Scale -> Scale
extractTriad root scale =
  let convert idx = getPCList scale !! (idx `mod` getSize scale)
      cRoot = convert root
  in  mkScale cRoot [convert (root + 2) - cRoot, convert (root + 4) - cRoot]

extractSeventh :: Int -> Scale -> Scale
extractSeventh root scale =
  let convert idx = getPCList scale !! (idx `mod` getSize scale)
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
