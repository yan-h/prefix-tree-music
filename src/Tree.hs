{-# LANGUAGE TemplateHaskell, FlexibleInstances, FunctionalDependencies #-}

module Tree where

import           Event 

import           Euterpea

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Control.Lens            hiding ( Choice )
import           Debug.Trace
import           Data.List                      ( foldl'
                                                , sort
                                                )

data Orientation = H | V
  deriving Show

data OrientedTree a =
    Val a
  | Group Orientation [OrientedTree a]
  deriving (Show)

instance Functor OrientedTree where
  fmap f (Val a     ) = Val (f a)
  fmap f (Group H as) = Group H (map (fmap f) as)
  fmap f (Group V as) = Group V (map (fmap f) as)

--------------------------------------------------------------------------------
-- Slice
--------------------------------------------------------------------------------

type Path = [Int]

type Slice = [Choice]

data Choice =  Some [Int]
             | All
  deriving (Eq, Show)

setAtIndex :: Int -> a -> [a] -> [a]
setAtIndex _ _ [] = []
setAtIndex idx val (x : xs) | idx < 0   = x : xs
                            | idx == 0  = val : xs
                            | otherwise = x : setAtIndex (idx - 1) val xs

atHands, atPeriods, atPhrases, atMeasures, atChords, atVoices, atNotes
  :: [Int] -> Slice -> Slice
atHands = setAtIndex 0 . Some
atPeriods = setAtIndex 1 . Some
atPhrases = setAtIndex 2 . Some
atMeasures = setAtIndex 3 . Some
atChords = setAtIndex 4 . Some
atVoices = setAtIndex 5 . Some
atNotes = setAtIndex 6 . Some

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
  foldl' (flip applyModifier) (startingTree modifiers) modifiers

toTreeStructure :: [TreeModifier] -> TreeStructure
toTreeStructure = foldr (addSlice . _slice) TUnit

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

--------------------------------------------------------------------------------
-- TreeShape 
--------------------------------------------------------------------------------

data TreeShape =
  TAll TreeShape
  | TSome [TreeShape]
  | TLeaf
  deriving (Show)

extendList :: Int -> a -> [a] -> [a]
extendList n e xs | n <= length xs = xs
                  | otherwise      = xs ++ replicate (n - length xs) e

mapChoice :: [Int] -> (a -> a) -> [a] -> [a]
mapChoice idxs f as =
  zipWith (\a idx -> if idx `elem` idxs then f a else a) as [0 ..]

startingTree :: [TreeModifier] -> OrientedTree Event
startingTree modifiers =
  let slices        = map _slice modifiers
      treeStructure = foldr addSlice TTLeaf slices
  in  toDefaultOrientedTree treeStructure

toDefaultOrientedTree :: TreeShape -> OrientedTree Event
toDefaultOrientedTree =
  go $ [Group V, Group H, Group H, Group H, Group H, Group V, Group H] ++ repeat
    (Group H)
 where
  go (c : cs) TLeaf      = Val defaultEvent
  go (c : cs) (TAll  t ) = c [go cs t]
  go (c : cs) (TSome ts) = c . map (go cs) $ ts

addSlice :: Slice -> TreeShape -> TreeShape
addSlice []          t          = t
addSlice (TAll : xs) TLeaf      = TAll (addSlice xs TLeaf)
addSlice (TAll : xs) (TAll  t ) = TAll (addSlice xs t)
addSlice (TAll : xs) (TSome ts) = TSome (map (addSlice xs) ts)
addSlice (TSome is : xs) TLeaf =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) TLeaf))
addSlice (TSome is : xs) (TAll t) =
  TSome (mapChoice is (addSlice xs) (replicate (maximum is + 1) t))
addSlice (TSome is : xs) (TSome ts) =
  TSome (mapChoice is (addSlice xs) (extendList (maximum is + 1) TLeaf ts))

