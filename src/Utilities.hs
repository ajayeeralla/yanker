
module Utilities where

import Data.List as List
import Data.Set as Set
import Data.Maybe as Maybe

seqInt :: Int -> [Int] -> [Int]
seqInt 0 accu = accu
seqInt n accu = seqInt (n-1) (n:accu)

doList f = List.foldl (\ accu elem -> accu >> (f elem)) (return ())
doSet f = Set.foldl (\ accu elem -> accu >> (f elem)) (return ())

type Vec2 = (Double,Double)

-- Cross product
cross :: Vec2 -> Vec2 -> Double
cross (xa,ya) (xb,yb) =
  xa*yb - ya*xb

-- l2 norm
l2norm :: Vec2 -> Double
l2norm (x,y) =
  sqrt $ x*x + y*y

-- Divide by the norm
toUnitary :: Vec2 -> Vec2
toUnitary (x,y) =
  (x/n,y/n)
  where
    n = l2norm (x,y)

-- Difference of two vectors
diff2 :: Vec2 -> Vec2 -> Vec2
diff2 (xa,ya) (xb,yb) =
  (xa-xb, ya-yb)


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

---------------------------
-- Set-related utilities --
---------------------------

-- For all, for sets
allSet :: Ord a => (a -> Bool) -> Set.Set a -> Bool
allSet pred = Set.foldl' (\ found elem -> found && pred elem) True

-- Flatten a Set of Sets
concatSet :: Ord a => Set.Set (Set.Set a) -> Set.Set a
concatSet = Set.unions . Set.elems

-- Number of elements satisfying a predicate
countSet :: Ord a => (a -> Bool) -> Set.Set a -> Int
countSet f = Set.size . Set.filter f

-- Find an element in a set satisfying a predicate
findSet :: Ord a => (a -> Bool) -> Set a -> Maybe a
findSet predicate = Set.foldl (\ accu elem -> if (predicate elem) then Just elem else accu) Nothing
