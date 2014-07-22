
module Utilities where

import Data.List as List
import Data.Set as Set

seqInt :: Int -> [Int] -> [Int]
seqInt 0 accu = accu
seqInt n accu = seqInt (n-1) (n:accu)

doList f = List.foldl (\ accu elem -> accu >> (f elem)) (return ())
doSet f = Set.foldl (\ accu elem -> accu >> (f elem)) (return ())
