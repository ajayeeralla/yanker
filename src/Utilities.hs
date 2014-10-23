
module Utilities where

import Data.List as List
import Data.Set as Set

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