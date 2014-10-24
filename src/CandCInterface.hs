module CandCInterface where

import Text.XML.HXT.Core
import Text.XML.HXT.DOM.QualifiedName
import Data.Tree.NTree.TypeDefs

import Data.Set as Set
import Data.Maybe
import Data.List as List

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

qNameCCG = mkName "ccg"
qNameRule = mkName "rule"
qNameLF = mkName "lf"

parseXMLderivation :: [Int] -> XmlTree -> Set (Int,Int)
parseXMLderivation offsets tree =
  recurse tree
  where
  recurse (NTree (XTag name attrs) children) =
    if name == qNameLF then
      Set.empty
    else if name == qNameCCG then
      mapM_ recurse children
    else if name == qNameRule then
      case children of
        [unary] -> recurse unary
        [left,right] ->
          case ruleType attrs of
            Just "fa" -> -- forward application
              Set.empty -- TODO
            Just "ba" -> -- backward application
              Set.empty
            Just "lp" -> -- left punctuation
              recurse right
            Just "rp" -> -- right punctuation
              recurse left
            Just x -> error "Not implemented"
    else
      error "Invalid XML derivation"
  ruleType :: XmlTrees -> Maybe String
  ruleType = do
    (NTree _ children) <- List.find $
             \ (NTree root children) -> root == XTag (mkName "type")
    (NTree val _) <- listToMaybe children
    case val of
      XText str -> return str
      otherwise -> Nothing


