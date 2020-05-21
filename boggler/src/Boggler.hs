{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

module Boggler where

import Prelude hiding (readFile, lookup, null, length)
import Data.List hiding (lookup, null, length)
import Data.Ord (comparing)
import Data.ByteString (readFile, split, ByteString, length)
import qualified Data.ByteString.Char8 as C
import qualified Data.Trie as T
import qualified Data.Map as M
import Data.Maybe
import Data.Functor.Foldable
import Data.List.NonEmpty (NonEmpty(..), fromList, (<|))

-------------------------------------------------------------------------------
-- Board

type Coord = (Integer, Integer)
type Path = NonEmpty Coord
type Board = M.Map Coord ByteString

boardCoords = [(x, y) | x <- [1..4], y <- [1..4]]

nextCoords :: Path -> [Coord]
nextCoords ((a,b):|xs) = filter (`notElem`xs) $ filter bounded around
  where
    around = delete (a,b) [(a + x, b + y) | x <- [-1..1], y <- [-1..1]]
    bounded (x,y) = and [0 < x, x < 5, 0 < y, y < 5]

-------------------------------------------------------------------------------
-- Fix Recursion

-- Set up a tree with Fix so that will have a Base instance for use by
-- recursion scheme functions.
data TrF a r = Nd a [r] deriving (Show, Functor)
type Tr a = Fix (TrF a)

-- A hylomoporphic function that generates a list of all possible words from a
-- given starting position. alg builds up a tree of valid word paths, and coalg
-- collapses that down to a flat list.
haichBoy = hylo coalg alg
  where
    alg (b, p@(c:|_), t) =
      let (w, t') = T.lookupBy (,) (b M.! c) t
          cs = map (<|p) $ nextCoords p
      in Nd w $ if not $ T.null t' then map (b,,t') cs else []
    coalg (Nd p cs) = maybeToList p ++ concat cs

getBest :: T.Trie ByteString -> Board -> ByteString
getBest t b = maximumBy (comparing length)
            $ concatMap (haichBoy . (b,,t))
            $ map (fromList . (:[])) boardCoords
