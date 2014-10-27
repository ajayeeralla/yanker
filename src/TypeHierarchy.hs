{-# LANGUAGE DeriveGeneric #-}

module TypeHierarchy where

import Data.List
import Data.Functor
import Data.Monoid
import Data.Binary
import Data.Serialize
import GHC.Generics as GGen

import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr as PE
import Text.ParserCombinators.Parsec.Language

import qualified Data.Map.Strict as Map

-- Base types with annotations
data AnnotT = AnnotT { baseType :: String, annotType :: Maybe String }
              deriving (Eq,Show,Ord,GGen.Generic)
instance Binary AnnotT

renderAnnotT :: AnnotT -> String
renderAnnotT (AnnotT base Nothing) = base
renderAnnotT (AnnotT base (Just annot)) = base ++ "[" ++ annot ++ "]"

--moreGeneralThan :: AnnotT -> AnnotT -> Bool
--moreGeneralThan (AnnotT baseA Nothing) (AnnotT baseB t) =
--  baseA == baseB
--moreGeneralThan x y =
--  x == y
------------------------------------
-- Lambek types, without products --
------------------------------------
data LambekFun =
       LFAtom AnnotT
     | LFLeft LambekFun LambekFun
     | LFRight LambekFun LambekFun
   deriving (Eq,Show,Ord,GGen.Generic)
instance Binary LambekFun

-- Pretty printing
data ParenthesisNeeded = NoParen | AlwaysParen | ParenLeft | ParenRight
addParen str = "("++str++")"

renderLFparen (LFAtom base) _ = renderAnnotT base

renderLFparen (LFLeft b a) NoParen =
  (renderLFparen a AlwaysParen) ++ "\\" ++ (renderLFparen b ParenRight)
renderLFparen t@(LFLeft _ _) ParenRight = renderLFparen t NoParen
renderLFparen t@(LFLeft _ _) _ = addParen $ renderLFparen t NoParen

renderLFparen (LFRight a b) NoParen =
  (renderLFparen a ParenLeft) ++ "/" ++ (renderLFparen b AlwaysParen)
renderLFparen t@(LFRight _ _) ParenLeft = renderLFparen t NoParen
renderLFparen t@(LFRight _ _) _ = addParen $ renderLFparen t NoParen

renderLF x = renderLFparen x NoParen

typeLengthLF :: LambekFun -> Int
typeLengthLF (LFAtom _) = 1
typeLengthLF (LFLeft body arg) =
  typeLengthLF body + (typeLengthLF arg)
typeLengthLF (LFRight body arg) =
  typeLengthLF body + (typeLengthLF arg)

-- Lambek skeletons (without products)
data LambekSkel =
      LSAtom AnnotT
    | LSVar Int
    | LSLeft LambekSkel LambekSkel
    | LSRight LambekSkel LambekSkel
   deriving (Eq,Show,Ord,GGen.Generic)
instance Binary LambekSkel

-- Pretty printing

renderLSparen (LSAtom base) _ = renderAnnotT base

renderLSparen (LSVar n) _ = show n

renderLSparen (LSLeft b a) NoParen =
  (renderLSparen a AlwaysParen) ++ "\\" ++ (renderLSparen b ParenRight)
renderLSparen t@(LSLeft _ _) ParenRight = renderLSparen t NoParen
renderLSparen t@(LSLeft _ _) _ = addParen $ renderLSparen t NoParen

renderLSparen (LSRight a b) NoParen =
  (renderLSparen a ParenLeft) ++ "/" ++ (renderLSparen b AlwaysParen)
renderLSparen t@(LSRight _ _) ParenLeft = renderLSparen t NoParen
renderLSparen t@(LSRight _ _) _ = addParen $ renderLSparen t NoParen

renderLS x = renderLSparen x NoParen

-- Utility: unify two assignments
unionMap m1 m2 =
   Map.foldlWithKey
     (\ m2 key value -> m2 >>= (\m2 -> case Map.lookup key m2 of
        Nothing -> Just $ Map.insert key value m2
        Just otherVal ->
          if otherVal == value then
             Just $ Map.insert key value m2
          else Nothing))
     (Just m2)
     m1
     
-- Does this lambek type matches this Lambek skeleton ? If no, Nothing. Else, Just the corresponding type assignment
matchSkeleton :: LambekSkel -> LambekFun -> Maybe (Map.Map Int LambekFun)
matchSkeleton x@(LSAtom a) y@(LFAtom b) =
   if a == b then Just Map.empty else Nothing
matchSkeleton (LSVar x) t = Just $ Map.insert x t Map.empty
matchSkeleton (LSLeft a1 b1) (LFLeft a2 b2) = do
   s1 <- matchSkeleton a1 a2
   s2 <- matchSkeleton b1 b2
   unionMap s1 s2
matchSkeleton (LSRight a1 b1) (LFRight a2 b2) = do
   s1 <- matchSkeleton a1 a2
   s2 <- matchSkeleton b1 b2
   unionMap s1 s2
matchSkeleton _ _ = Nothing

-- More convenient syntax
isMatchedBy :: LambekFun -> LambekSkel -> Bool
isMatchedBy fun skel = matchSkeleton skel fun /= Nothing

-- Given a list of skeletons and a list of types
-- filter the types with the skeletons (each type is paired with
-- the first skeleton it meets)
dispatchTypes :: (a -> LambekFun) -> [LambekSkel] -> [a] -> ([[a]],[a])
dispatchTypes getType skels types =
  reverseOutput $ foldl addTypeToAccu (initSlots,[]) types
  where
    initSlots = map (\x -> (x,[])) skels
    addTypeToAccu (slots,noMatch) tp =
      case findMatching slots tp of
        Just newSlots -> (newSlots,noMatch)
        Nothing -> (slots,tp:noMatch)
    findMatching [] _ = Nothing
    findMatching ((skel,slot):tail) tp =
      if matchSkeleton skel (getType tp) /= Nothing then
        Just $ (skel,tp:slot):tail
      else do
        rest <- findMatching tail tp
        return $ (skel,slot):rest
    reverseOutput (slots,noMatch) =
      (map (reverse . snd) slots, reverse noMatch)

-- Lambek types, with products
data LambekType = LTAtom String | LTLeft LambekType LambekType | LTRight LambekType LambekType | LTProd LambekType LambekType
     deriving (GGen.Generic)
instance Binary LambekType

-- instance Semigroup LambekType where
--   (<>) = LTProd

------- PARSING ----------

lexerL = P.makeTokenParser (emptyDef { reservedOpNames = ["/","\\","[","]"] })
whiteSpaceL = P.whiteSpace lexerL
myIdentifier = many (noneOf "/\\[],\n")

termLF :: Parser LambekFun
termLF = (atomLF LFAtom) <|> (P.parens lexerL parserLF)

atomLF atomFun = do
  id <- (P.identifier lexerL)
  bracketParser id <|> (return $ atomFun (AnnotT id Nothing))
  where
    bracketParser baseId = do
      char '['
      annot <- myIdentifier
      char ']'
      return $ atomFun (AnnotT baseId (Just annot))

termLFwhiteSpace = whiteSpaceL >> termLF

tableLF = [ [PE.Infix (whiteSpaceL >> char '/' >> return LFRight) AssocLeft ],
            [PE.Infix (whiteSpaceL >> char '\\' >> return LFLeft) AssocRight ] ]

parserLF = buildExpressionParser tableLF termLFwhiteSpace

parserLFeof = do
   whiteSpaceL
   x <- parserLF
   whiteSpaceL
   eof
   return x

parseLFFromString :: String -> Either String LambekFun
parseLFFromString input = case parse parserLFeof "" input of
  Left error -> Left (show error)
  Right something -> Right something

---- Parsing for skeletons ----

termLS :: Parser LambekSkel
termLS = natParser <|> (atomLF LSAtom) <|> (P.parens lexerL parserLS)
    where
      natParser = do
        x <- P.natural lexerL
        return $ LSVar (fromIntegral x)

termLSwhiteSpace = whiteSpaceL >> termLS

tableLS = [ [PE.Infix (whiteSpaceL >> char '/' >> return LSRight) AssocLeft ],
            [PE.Infix (whiteSpaceL >> char '\\' >> return (\a b -> LSLeft b a)) AssocRight ] ]

parserLS = buildExpressionParser tableLS termLSwhiteSpace

parserLSeof = do
  whiteSpaceL
  x <- parserLS
  whiteSpaceL
  eof
  return x
  
--------------------
-- Pregroup types --
--------------------

-- Simple type in a pregroup: a base type and an exponent. +1 means right, -1 means left
data PrgSType = PrgS AnnotT Int
        deriving (Show, Eq, GGen.Generic)
instance Binary PrgSType

-- Simple left adjoint
leftAdjS (PrgS base exp) = PrgS base (exp-1)
-- Simple right adjoint 
rightAdjS (PrgS base exp) = PrgS base (exp+1)
-- Pretty printing
renderST (PrgS base 0) = renderAnnotT base
renderST (PrgS base n) =
   let exponent = (if n > 0 then replicate n 'r' else replicate (-n) 'l')
   in
   (renderAnnotT base) ++ "(" ++ exponent ++ ")"

-- (Complex) type in a pregroup: list of simple types
type PrgType = [PrgSType]

-- Left adjoint
leftAdj :: PrgType -> PrgType
leftAdj = reverse . map leftAdjS
-- Right adjoint
rightAdj :: PrgType -> PrgType
rightAdj = reverse . map rightAdjS
-- Pretty printing
renderPrg :: PrgType -> String
renderPrg = foldl (\accu e -> renderST e ++ " " ++ accu) ""

-- Canonical morphism from Lambek to pregroup
lambekToPregroup :: LambekFun -> PrgType
lambekToPregroup (LFAtom typ) = [PrgS typ 0]
lambekToPregroup (LFLeft body arg) =
  (lambekToPregroup body) ++ (leftAdj . lambekToPregroup $ arg)
lambekToPregroup (LFRight body arg) =
  (rightAdj . lambekToPregroup $ arg) ++ (lambekToPregroup body)
       
-----------------
-- Group types --
-----------------

-- Simple type in a group: a base type and its exponent (True for +1, False for -1)
data GrpSType = GrpS AnnotT Bool
        deriving (GGen.Generic)
instance Binary GrpSType

-- Inverse
invGrpS (GrpS base exp) = GrpS base (not exp)
-- Pretty printing
renderGS (GrpS base True) = renderAnnotT base
renderGS (GrpS base False) = (renderAnnotT base) ++ "(-1)"

-- (Complex) type in a group: product of simple types
type GrpType = [GrpSType]

-- Inverse
invGrp = reverse . (map invGrpS)
-- Pretty printing
renderGrp = foldl (\ accu elem -> accu ++ " " ++ (renderGS elem)) ""

-- Canonical morphism from pregroup to group
-- Simple version
pregroupToGroupS :: PrgSType -> GrpSType
pregroupToGroupS (PrgS base exp) = GrpS base (exp `rem` 2 == 0)

-- Complex version
pregroupToGroup :: PrgType -> GrpType
pregroupToGroup =
  map pregroupToGroupS

-- Helper
lambekToGroup :: LambekFun -> GrpType
lambekToGroup =
  pregroupToGroup . lambekToPregroup
