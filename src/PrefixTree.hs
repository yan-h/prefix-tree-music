{-# LANGUAGE TemplateHaskell, FlexibleInstances, FunctionalDependencies #-}

module PrefixTree where

import           ScalePitch

import           Euterpea

import           Data.Tree

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Lens
import           Debug.Trace
import           Data.List                      ( foldl' )

data OrientedTree a =
    Leaf a
  | HBranch [OrientedTree a]
  | VBranch [OrientedTree a]
  deriving (Show)

instance Functor OrientedTree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (HBranch as) = HBranch (map (fmap f) as)
  fmap f (VBranch as) = VBranch (map (fmap f) as)


type Path = [Int]

type Slice = [[Int]]

sliceToPaths :: Slice -> [Path]
sliceToPaths [] = [[]]
sliceToPaths (choices : rest) =
  [ x : xs | x <- choices, xs <- sliceToPaths rest ]

data Event = Event
  { _duration :: Dur
  , _volume :: Volume
  , _scalePitch :: ScalePitch
  }
  deriving (Show)

makeLenses ''Event

toString :: Primitive (AbsPitch, Volume) -> String
toString (Note dur (p, v)) = "note " ++ show p ++ " (" ++ show dur ++ ") " ++ show v
toString (Rest dur) = "rest " ++ show dur

eventToPitch :: Event -> AbsPitch 
eventToPitch = getPitch . view scalePitch

toPrimitive :: Event -> Primitive (AbsPitch, Volume)
toPrimitive (Event dur vol (ScalePitch scale octave degree))
  | vol == 0
  = Rest dur
  | otherwise
  = let absPitch = getPitch $ mkScalePitch scale octave degree
    in  Note dur (absPitch, vol)

data TreeModifier = TreeModifier
  { _slice :: Slice
  , _modifier :: Event -> Event
  }

instance Show TreeModifier where
  show (TreeModifier slice modifier) = show slice

defaultEvent :: Event
defaultEvent = Event 10 10 (ScalePitch (mkMajorScale 0) 0 0)

defaultTreeModifier :: TreeModifier
defaultTreeModifier = TreeModifier (replicate 7 [0]) id

setAtIndex :: Int -> a -> [a] -> [a]
setAtIndex _ _ [] = []
setAtIndex idx val (x : xs) | idx < 0   = x : xs
                            | idx == 0  = val : xs
                            | otherwise = x : setAtIndex (idx - 1) val xs

setAtSliceIndex :: Int -> [Int] -> TreeModifier -> TreeModifier
setAtSliceIndex sliceIndex val (TreeModifier slice event) =
  TreeModifier (setAtIndex sliceIndex val slice) event

atHands, atPeriods, atPhrases, atMeasures, atChords, atVoices, atNotes
  :: [Int] -> TreeModifier -> TreeModifier
atHands = setAtSliceIndex 0
atPeriods = setAtSliceIndex 1
atPhrases = setAtSliceIndex 2
atMeasures = setAtSliceIndex 3
atChords = setAtSliceIndex 4
atVoices = setAtSliceIndex 5
atNotes = setAtSliceIndex 6

composeModifier :: (Event -> Event) -> TreeModifier -> TreeModifier
composeModifier fn (TreeModifier slice modifier) =
  TreeModifier slice (fn . modifier)

modifyDuration :: (Dur -> Dur) -> TreeModifier -> TreeModifier
modifyDuration fn = composeModifier (over duration fn)

modifyVolume :: (Int -> Int) -> TreeModifier -> TreeModifier
modifyVolume fn = composeModifier (over volume fn)

modifyScale :: (Scale -> Scale) -> TreeModifier -> TreeModifier
modifyScale fn = composeModifier (over (scalePitch . scale) fn)

modifyOctave :: (Int -> Int) -> TreeModifier -> TreeModifier
modifyOctave fn = composeModifier (over (scalePitch . octave) fn)

modifyDegree :: (Int -> Int) -> TreeModifier -> TreeModifier
modifyDegree fn = composeModifier (over (scalePitch . degree) fn)

modifyAbsDegree :: (Int -> Int) -> TreeModifier -> TreeModifier
modifyAbsDegree fn = composeModifier (over (scalePitch . absDegree) fn)

setDuration :: Dur -> TreeModifier -> TreeModifier
setDuration val = modifyDuration (const val)

setVolume :: Int -> TreeModifier -> TreeModifier
setVolume val = modifyVolume (const val)

setScale :: Scale -> TreeModifier -> TreeModifier
setScale val = modifyScale (const val)

setOctave :: Octave -> TreeModifier -> TreeModifier
setOctave val = modifyOctave (const val)

setDegree :: Int -> TreeModifier -> TreeModifier
setDegree val = modifyDegree (const val)

leaf :: a -> Tree a
leaf a = Node a []

toTreeModifiers :: Tree (TreeModifier -> TreeModifier) -> [TreeModifier]
toTreeModifiers = go defaultTreeModifier
 where
  go acc (Node fn children) =
    let newAcc = fn acc
    in  case children of
          [] -> [newAcc]
          _  -> concatMap (go newAcc) children

applyModifier :: Event -> Map Path Event -> TreeModifier -> Map Path Event 
applyModifier defEvt treeMap (TreeModifier slice modifier) = foldl'
  (\curMap path -> Map.alter updatePartialEvent path curMap)
  treeMap
  (sliceToPaths slice)
  where
    updatePartialEvent :: Maybe Event -> Maybe Event
    updatePartialEvent Nothing    = Just (modifier defEvt)
    updatePartialEvent (Just evt) = Just (modifier evt)

toEvents :: [TreeModifier] -> Map Path Event
toEvents = foldl' (applyModifier defaultEvent) Map.empty

firstKey :: Map k a -> Maybe k
firstKey m = fst <$> firstElem m
 where
  firstElem :: Map k a -> Maybe (k, a)
  firstElem m = case Map.toAscList m of
    []      -> Nothing
    (x : _) -> Just x

toOrientedTreeBase
  :: [  [OrientedTree (Primitive (AbsPitch, Volume))]
     -> OrientedTree (Primitive (AbsPitch, Volume))
     ]
  -> Map Path Event
  -> OrientedTree (Primitive (AbsPitch, Volume))
toOrientedTreeBase [] table = case Map.toAscList table of
  [(_, evt)] -> Leaf $ toPrimitive evt
  _          -> error "Not enough levels provided to toTreeModifiers"
toOrientedTreeBase (fn : fns) table = 
  case newLevel of 
    [x] -> x
    _ -> fn newLevel
 where
  newLevel = splitLevel table
  splitLevel :: Map Path Event -> [OrientedTree (Primitive (AbsPitch, Volume))]
  splitLevel table = case firstKey table of
    Nothing -> []
    Just (minKey : _) ->
      let (t1, t2) = Map.spanAntitone (\k -> head k <= minKey) table
      in  toOrientedTreeBase fns (Map.mapKeys tail t1) : splitLevel t2

toOrientedTree :: Map Path Event -> OrientedTree (Primitive (AbsPitch, Volume))
toOrientedTree = toOrientedTreeBase
  [VBranch, HBranch, HBranch, HBranch, HBranch, VBranch, HBranch]

toMusic :: OrientedTree (Primitive a) -> Music a
toMusic (Leaf    x ) = Prim x
toMusic (HBranch ts) = line1 (map toMusic ts) -- Assumes nonempty branches
toMusic (VBranch ts) = chord1 (map toMusic ts)

toPlayable :: Tree (TreeModifier -> TreeModifier) -> Music (AbsPitch, Volume)
toPlayable = toMusic . toOrientedTree . toEvents . toTreeModifiers
