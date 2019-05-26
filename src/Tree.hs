{-# LANGUAGE FlexibleInstances, DeriveFunctor #-}

module Tree where

import           Event

import           Control.Lens                   ( view
                                                , over
                                                , set, ix
                                                )
import           Euterpea

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List                      ( foldl'
                                                , sort
                                                )


--------------------------------------------------------------------------------
-- OrientedTree 
--------------------------------------------------------------------------------

data Orientation = H | V
  deriving Show

data OrientedTree a =
    Val a
  | Group Orientation [OrientedTree a]
  deriving (Show, Functor)

--------------------------------------------------------------------------------
-- Slice
--------------------------------------------------------------------------------

type Slice = [Choice]

data Choice =  Some [Int]
             | All
  deriving (Eq, Show)

atHands, atPeriods, atPhrases, atMeasures, atChords, atVoices, atNotes
  :: [Int] -> Slice -> Slice
atHands = set (ix 0) . Some
atPeriods = set (ix 1) . Some
atPhrases = set (ix 2) . Some
atMeasures = set (ix 3) . Some
atChords = set (ix 4) . Some
atVoices = set (ix 5) . Some
atNotes = set (ix 6) . Some

--------------------------------------------------------------------------------
-- TreeModifier
--------------------------------------------------------------------------------

data TreeModifier = TreeModifier
  { _slice :: Slice
  , _modifier :: Event -> Event
  }

instance Show TreeModifier where
  show (TreeModifier slice modifier) = show slice

defaultTreeModifier :: TreeModifier
defaultTreeModifier = TreeModifier (replicate 7 All) id

--------------------------------------------------------------------------------
-- PrefixTree 
--------------------------------------------------------------------------------

data PrefixTree a b =
    Leaf a b
  | Node a [PrefixTree a b]

type MusicTree = PrefixTree (Slice -> Slice) (Event -> Event)

toTreeModifiers
  :: PrefixTree (Slice -> Slice) (Event -> Event) -> [TreeModifier]
toTreeModifiers = go (replicate 7 All)
 where
  go :: Slice -> PrefixTree (Slice -> Slice) (Event -> Event) -> [TreeModifier]
  go curSlice (Node sliceMod children) =
    let newSlice = sliceMod curSlice
    in  case children of
          [] -> []
          _  -> concatMap (go newSlice) children
  go curSlice (Leaf sliceMod eventMod) =
    let newSlice = sliceMod curSlice in [TreeModifier newSlice eventMod]

toOrientedTree :: [TreeModifier] -> OrientedTree Event
toOrientedTree modifiers =
  let startingTree :: OrientedTree Event
      startingTree = makeStartingTree modifiers
  in  foldl' (flip applyModifier) startingTree modifiers

applyModifier :: TreeModifier -> OrientedTree Event -> OrientedTree Event
applyModifier (TreeModifier slice f) = go slice
 where
  go _               (Val e     ) = Val (f e)
  go (All     : iss) (Group o ts) = Group o $ map (go iss) ts
  go (Some is : iss) (Group o ts) = Group o
    $ zipWith (\t idx -> if idx `elem` is then go iss t else t) ts [0 ..]

toMusic :: OrientedTree Event -> Music (AbsPitch, Volume)
toMusic (Val x     ) = Prim (toPrimitive x)
toMusic (Group H ts) = line1 (map toMusic ts) -- Assumes nonempty branches
toMusic (Group V ts) = chord1 (map toMusic ts)

toPlayable :: MusicTree -> Music (AbsPitch, Volume)
toPlayable = toMusic . toOrientedTree . toTreeModifiers

--------------------------------------------------------------------------------
-- TreeShape 
--------------------------------------------------------------------------------

-- Used only to infer a skeletal 'OrientedTree' from a musical prefix tree.
data TreeShape =
  TAll TreeShape
  | TSome [TreeShape]
  | TLeaf
  deriving Show

makeStartingTree :: [TreeModifier] -> OrientedTree Event
makeStartingTree modifiers =
  let slices        = map _slice modifiers
      treeStructure = foldr addSlice TLeaf slices
  in  toDefaultOrientedTree treeStructure

extendList :: Int -> a -> [a] -> [a]
extendList n e xs | n <= length xs = xs
                  | otherwise      = xs ++ replicate (n - length xs) e

mapChoice :: [Int] -> (a -> a) -> [a] -> [a]
mapChoice idxs f as =
  zipWith (\a idx -> if idx `elem` idxs then f a else a) as [0 ..]

toDefaultOrientedTree :: TreeShape -> OrientedTree Event
toDefaultOrientedTree =
  go $ [Group V, Group H, Group H, Group H, Group H, Group V, Group H] ++ repeat
    (Group H)
 where
  go (c : cs) TLeaf      = Val defaultEvent
  go (c : cs) (TAll  t ) = c [go cs t]
  go (c : cs) (TSome ts) = c . map (go cs) $ ts

addSlice :: Slice -> TreeShape -> TreeShape
addSlice []         t          = t
addSlice (All : xs) TLeaf      = TAll (addSlice xs TLeaf)
addSlice (All : xs) (TAll  t ) = TAll (addSlice xs t)
addSlice (All : xs) (TSome ts) = TSome (map (addSlice xs) ts)
addSlice (Some is : xs) TLeaf =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) TLeaf))
addSlice (Some is : xs) (TAll t) =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) t))
addSlice (Some is : xs) (TSome ts) =
  TSome (mapChoice is (addSlice xs) (extendList (maximum is + 1) TLeaf ts))
