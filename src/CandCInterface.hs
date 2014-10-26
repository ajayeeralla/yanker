module CandCInterface where

import Text.XML.HXT.Core
import Text.XML.HXT.DOM.QualifiedName
import Data.Tree.NTree.TypeDefs

import Data.Set as Set
import Data.Maybe
import Data.List as List
import Data.Bimap

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

type Bindings = Bimap Int Int

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
      (Set.empty,currentType)
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
              (Set.union cancel bindL,currentType) -- TODO
            Just "ba" -> -- backward application
              (Set.empty,currentType)
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


