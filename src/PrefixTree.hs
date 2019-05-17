{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module PrefixTree where

import           ScalePitch

import           Euterpea

import           Data.Tree

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Lens

data OrientedTree a =
    Leaf a
  | HBranch [OrientedTree a]
  | VBranch [OrientedTree a]
  deriving (Show)

type Path = [Int]

type Slice = [[Int]]

sliceToPaths :: Slice -> [Path]
sliceToPaths [] = [[]]
sliceToPaths (choices : rest) =
  [ x : xs | x <- choices, xs <- sliceToPaths rest ]

{--
data Event' a b c d e =
  NoteEvent
  { _duration :: Dur
  , _volume :: Volume
  , _scalePitch :: ScalePitch
  }
  | RestEvent 
  { _duration :: Dur }
  --}

data PartialEvent = PartialEvent
  { _maybeDuration :: Maybe Rational
  , _maybeVolume :: Maybe Int
  , _maybeScale :: Maybe Scale
  , _maybeOctave :: Maybe Octave
  , _maybeDegree :: Maybe Degree
  , _modifier :: Event -> Event
  }

data Event = NoteEvent
  { _duration :: Dur
  , _volume :: Volume
  , _scalePitch :: ScalePitch
  }
  | RestEvent 
  { _duration :: Dur }

makeFieldsNoPrefix ''PartialEvent
makeFieldsNoPrefix ''Event
makePrisms ''Event

toPrimitive :: Event -> Primitive (AbsPitch, Volume)
toPrimitive (RestEvent dur) = Rest dur
toPrimitive (NoteEvent dur vol (ScalePitch scale octave degree)) =
  let absPitch = getPitch $ mkScalePitch scale octave degree
  in  Note dur (absPitch, vol)

toEvent :: PartialEvent -> Event
toEvent (PartialEvent (Just dur) (Just 0) _ _ _ trans) = trans $ RestEvent dur
toEvent (PartialEvent (Just dur) (Just vol) (Just scale) (Just octave) (Just degree) trans)
  = trans $ NoteEvent dur vol (mkScalePitch scale octave degree)

{--
toEvent :: PartialEvent -> Primitive (AbsPitch, Volume)
toEvent (PartialEvent (Just dur) (Just (-1)) _ _ _) = Rest dur
toEvent (PartialEvent (Just dur) (Just vol) (Just scale) (Just octave) (Just degree))
  = let absPitch = getPitch $ mkScalePitch scale octave degree
    in  Note dur (absPitch, vol)
toEvent _ = error "Incomplete PartialEvent"
--}
data PartialTree = PartialTree
  { _slice :: Slice
  , _partialEvent :: PartialEvent
  }

-- Merges two 'Maybe's. Prefers the first.
mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe (Just a) (Just a') = Just a
mergeMaybe (Just a) Nothing   = Just a
mergeMaybe Nothing  (Just a)  = Just a
mergeMaybe _        _         = Nothing

mergePartialEvents :: PartialEvent -> PartialEvent -> PartialEvent
mergePartialEvents x y = PartialEvent
  (mergeMaybe (_maybeDuration x) (_maybeDuration y))
  (mergeMaybe (_maybeVolume x) (_maybeVolume y))
  (mergeMaybe (_maybeScale x) (_maybeScale y))
  (mergeMaybe (_maybeOctave x) (_maybeOctave y))
  (mergeMaybe (_maybeDegree x) (_maybeDegree y))
  (_modifier x . _modifier y)

defaultPartialTree :: PartialTree
defaultPartialTree = PartialTree
  (replicate 7 [0])
  (PartialEvent Nothing Nothing Nothing Nothing Nothing id)

setAtIndex :: Int -> a -> [a] -> [a]
setAtIndex _ _ [] = []
setAtIndex idx val (x : xs) | idx < 0   = x : xs
                            | idx == 0  = val : xs
                            | otherwise = x : setAtIndex (idx - 1) val xs

setAtSliceIndex :: Int -> [Int] -> PartialTree -> PartialTree
setAtSliceIndex sliceIndex val (PartialTree slice event) =
  PartialTree (setAtIndex sliceIndex val slice) event

atHands, atPeriods, atPhrases, atMeasures, atChords, atVoices, atNotes
  :: [Int] -> PartialTree -> PartialTree
atHands = setAtSliceIndex 0
atPeriods = setAtSliceIndex 1
atPhrases = setAtSliceIndex 2
atMeasures = setAtSliceIndex 3
atChords = setAtSliceIndex 4
atVoices = setAtSliceIndex 5
atNotes = setAtSliceIndex 6

setDuration :: Rational -> PartialTree -> PartialTree
setDuration val (PartialTree slice partialEvent) =
  PartialTree slice (partialEvent { _maybeDuration = Just val })

setVolume :: Int -> PartialTree -> PartialTree
setVolume val (PartialTree slice partialEvent) =
  PartialTree slice (partialEvent { _maybeVolume = Just val })

setScale :: Scale -> PartialTree -> PartialTree
setScale val (PartialTree slice partialEvent) =
  PartialTree slice (partialEvent { _maybeScale = Just val })

setOctave :: Octave -> PartialTree -> PartialTree
setOctave val (PartialTree slice partialEvent) =
  PartialTree slice (partialEvent { _maybeOctave = Just val })

setDegree :: Degree -> PartialTree -> PartialTree
setDegree val (PartialTree slice partialEvent) =
  PartialTree slice (partialEvent { _maybeDegree = Just val })

addTransform :: (Event -> Event) -> PartialTree -> PartialTree
addTransform m' (PartialTree slice partialEvent@(PartialEvent _ _ _ _ _ m)) =
  PartialTree slice (partialEvent { _modifier = m' . m })

modifyAbsDegree :: (Int -> Int) -> Event -> Event
modifyAbsDegree = over (scalePitch . absDegree)

overAbsDegree :: (Int -> Int) -> PartialTree -> PartialTree
overAbsDegree = addTransform . modifyAbsDegree

leaf :: a -> Tree a
leaf a = Node a []

toPartialTrees :: Tree (PartialTree -> PartialTree) -> [PartialTree]
toPartialTrees = go defaultPartialTree
 where
  go acc (Node fn children) =
    let newAcc = fn acc
    in  case children of
          [] -> [newAcc]
          _  -> concatMap (go newAcc) children

toPartialEvents :: [PartialTree] -> Map Path PartialEvent
toPartialEvents = foldr go Map.empty where
  go :: PartialTree -> Map Path PartialEvent -> Map Path PartialEvent
  go pTree accMap =
    let paths  = sliceToPaths (_slice pTree)
        pEvent = _partialEvent pTree
        updatePartialEvent :: Maybe PartialEvent -> Maybe PartialEvent
        updatePartialEvent Nothing = Just pEvent
        updatePartialEvent (Just origPEvent) =
            Just (mergePartialEvents pEvent origPEvent)
    in  foldr (Map.alter updatePartialEvent) accMap paths

firstKey :: Map k a -> Maybe k
firstKey m = fst <$> firstElem m

firstElem :: Map k a -> Maybe (k, a)
firstElem m = case Map.toAscList m of
  []      -> Nothing
  (x : _) -> Just x

toOrientedTree
  :: [  [OrientedTree (Primitive (AbsPitch, Volume))]
     -> OrientedTree (Primitive (AbsPitch, Volume))
     ]
  -> Map Path PartialEvent
  -> OrientedTree (Primitive (AbsPitch, Volume))
toOrientedTree [] table = case Map.toAscList table of
  [(_, partialEvent)] -> HBranch [Leaf $ toPrimitive (toEvent partialEvent)]
  _                   -> error "Not enough levels provided to toPartialTrees"
toOrientedTree (fn : fns) table = fn (splitLevel table)
 where
  splitLevel
    :: Map Path PartialEvent -> [OrientedTree (Primitive (AbsPitch, Volume))]
  splitLevel table = case firstKey table of
    Nothing -> []
    Just (minKey : _) ->
      let (t1, t2) = Map.spanAntitone (\k -> head k <= minKey) table
      in  toOrientedTree fns (Map.mapKeys tail t1) : splitLevel t2

toMusic :: OrientedTree (Primitive a) -> Music a
toMusic (Leaf    x ) = Prim x
toMusic (HBranch ts) = line (map toMusic ts)
toMusic (VBranch ts) = chord (map toMusic ts)

toOrientedTree' =
  toOrientedTree [VBranch, HBranch, HBranch, HBranch, HBranch, VBranch, HBranch]

toPlayable
  :: Tree (PartialTree -> PartialTree) -> Music (AbsPitch, Volume)
toPlayable =
  toMusic . toOrientedTree' . toPartialEvents . toPartialTrees

