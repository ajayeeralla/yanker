module CandCInterface where

import Text.XML.HXT.Core
import Text.XML.HXT.DOM.QualifiedName
import Data.Tree.NTree.TypeDefs

import Data.Set as Set
import Data.Maybe
import Data.List as List
import Data.Map as Map

import TypeHierarchy

data ParseStructure =
  ParseStructure {bindings::[(Int,Int)], root::Int}

data CACTree =
    CACLeaf Int
  | CACFApp CACTree CACTree
  | CACBApp CACTree CACTree
  | CACFComp CACTree CACTree
  | CACBComp CACTree CACTree
  | CACTraise CACTree
    deriving (Eq,Show,Ord)

type Bindings = [(Int,Int)]

-- Computes the cancellation graph for a given type
-- left: indicates if we cancel using the left adjoint (otherwise, right adjoint)
-- right case: suppose the type is translated to a_1^{b_1} … a_n^{b_n) in a group
--  a_1^{b_1} … a_n^{b_n} a_n^{-b_n} … a_1^{-b_1}
--       \            \  |  /           /
--        \            --|--           /
--         \             |            /
--          -------------|------------
--                     offset
-- 
cancellationGraph :: Bool -> Int -> LambekFun -> Bindings
cancellationGraph left offset lambekType =
  let groupType = lambekToGroup lambekType in
  let inversed =
        if left then
          invGrp groupType
        else
          groupType
  in
  List.map (\(u,v) -> (u+offset,v+offset)) . computeGraph 1 inversed $ []
  where
    computeGraph :: Int -> GrpType -> Bindings -> Bindings
    computeGraph index [] bindings = bindings
    computeGraph index ((GrpS base exp):t) accu =
      computeGraph (index+1) t (newElem:accu)
      where
        newElem = if exp then (index,-index) else (-index,index)

qNameCCG = mkName "ccg"
qNameRule = mkName "rule"
qNameLF = mkName "lf"

parseXMLderivation :: XmlTree -> Bindings
parseXMLderivation tree =
  let (bindings,typ) = recurse 0 tree in
  bindings
  where
  recurse :: Int -> XmlTree -> (Bindings,LambekFun)
  recurse offset (NTree (XTag name attrs) children) =
    let typ = (maybeToEither "Attribute 'cat' not found in XML tag"
               (getAttr "cat" attrs)) >>= parseLFFromString in
    let currentType =
          case typ of
           Right x -> x
           Left er -> error er
    in
    let length = typeLengthLF currentType in
    
    if name == qNameLF then
      ([],currentType)
    else if name == qNameCCG then
      case children of
        [child] -> recurse offset child
        otherwise -> error "Expecting exactly one child for <ccg> tag"
    else if name == qNameRule then
      case children of
        [unary] -> recurse offset unary
        [left,right] ->
          let (bindL,typeL) = recurse offset left in
          let leftSize = typeLengthLF typeL in
          let (bindR,typeR) = recurse (offset+leftSize) right in
          case ruleType attrs of
            Just "fa" -> -- forward application
              let cancel = cancellationGraph True (offset+leftSize) typeR in
              (cancel ++ bindL,currentType) -- TODO
            Just "ba" -> -- backward application
              ([],currentType)
            Just "lp" -> -- left punctuation
              (bindR,typeR)
            Just "rp" -> -- right punctuation
              (bindL,typeL)
            Just x -> error "Not implemented"
    else
      error "Invalid XML derivation"
  asGroup :: LambekFun -> GrpType
  asGroup = pregroupToGroup . lambekToPregroup
  ruleType :: XmlTrees -> Maybe String
  ruleType = getAttr "type"
  getAttr :: String -> XmlTrees -> Maybe String
  getAttr attrName trees = do
    (NTree _ children) <- List.find 
             (\ (NTree root children) -> root == XAttr (mkName attrName)) trees
    (NTree val _) <- listToMaybe children
    case val of
      (XText str) -> return str
      otherwise -> Nothing
  
  maybeToEither :: a -> Maybe b -> Either a b
  maybeToEither message (Just x) = Right x
  maybeToEither message Nothing = Left message


