{-# LANGUAGE DeriveGeneric #-}

module TypeHierarchy where

import Data.List
import Data.Functor
import Data.Monoid
import Data.Serialize
import GHC.Generics as GGen

import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr as PE
import Text.ParserCombinators.Parsec.Language

import qualified Data.Map.Strict as Map
-- import Data.Semigroup

-- Simple type in a pregroup: a base type and an exponent. +1 means right, -1 means left
data PrgSType = PrgS String Int
        deriving (Show, Eq, GGen.Generic)

-- Left adjoint
leftAdj (PrgS base exp) = PrgS base (exp-1)
-- Right adjoint 
rightAdj (PrgS base exp) = PrgS base (exp+1)
-- Pretty printing
renderST (PrgS base 0) = base
renderST (PrgS base n) =
   let exponent = (if n > 0 then replicate n 'r' else replicate (-n) 'l')
   in
   base ++ "(" ++ exponent ++ ")"

-- (Complex) type in a pregroup: list of simple types
data PrgType = Prg [PrgSType]
        deriving (GGen.Generic)

-- Left adjoint
leftAdjC (Prg l) = (Prg (reverse (map leftAdj l)))
-- Right adjoint
rightAdjC (Prg l) = (Prg (reverse (map rightAdj l)))
-- Pretty printing
renderPrg (Prg t) = foldl (\accu e -> renderST e ++ " " ++ accu) "" t

instance Monoid PrgType where
  mempty = Prg []
  mappend = (\ (Prg a) (Prg b) -> Prg (a ++ b))

-- Simple type in a group: a base type and its exponent (True for +1, False for -1)
data GrpSType = GrpS String Bool
        deriving (GGen.Generic)

-- Inverse
invGrpS (GrpS base exp) = GrpS base (not exp)
-- Pretty printing
renderGS (GrpS base True) = base
renderGS (GrpS base False) = base ++ "(-1)"

-- (Complex) type in a group: product of simple types
data GrpType = Grp [GrpSType]
-- Inverse
invGrp = reverse . (map invGrpS)
-- Pretty printing
renderGrp = foldl (\ accu elem -> accu ++ " " ++ (renderGS elem)) ""

-- Lambek types, without products
data LambekFun =
       LFAtom { baseLF :: String, annotLF :: Maybe String }
     | LFLeft LambekFun LambekFun
     | LFRight LambekFun LambekFun
   deriving (Eq,Show,Ord,GGen.Generic)

-- Pretty printing
data ParenthesisNeeded = NoParen | AlwaysParen | ParenLeft | ParenRight
addParen str = "("++str++")"

renderLFparen (LFAtom s Nothing) _ = s
renderLFparen (LFAtom s (Just annot)) _ = s ++ "[" ++ annot ++ "]"

renderLFparen (LFLeft b a) NoParen =
  (renderLFparen a AlwaysParen) ++ "\\" ++ (renderLFparen b ParenRight)
renderLFparen t@(LFLeft _ _) ParenRight = renderLFparen t NoParen
renderLFparen t@(LFLeft _ _) _ = addParen $ renderLFparen t NoParen

renderLFparen (LFRight a b) NoParen =
  (renderLFparen a ParenLeft) ++ "/" ++ (renderLFparen b AlwaysParen)
renderLFparen t@(LFRight _ _) ParenLeft = renderLFparen t NoParen
renderLFparen t@(LFRight _ _) _ = addParen $ renderLFparen t NoParen

renderLF x = renderLFparen x NoParen

-- Lambek skeletons (without products)
data LambekSkel =
      LSAtom { baseLS :: String, annotLS :: Maybe String }
    | LSVar Int
    | LSLeft LambekSkel LambekSkel
    | LSRight LambekSkel LambekSkel
   deriving (Eq,Show,Ord,GGen.Generic)

-- Pretty printing

renderLSparen (LSAtom s Nothing) _ = s
renderLSparen (LSAtom s (Just annot)) _ = s ++ "[" ++ annot ++ "]"

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
matchSkeleton x@(LSAtom a annotA) y@(LFAtom b annotB) =
   if a == b && annotA == annotB then Just Map.empty else Nothing
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
  bracketParser id <|> (return $ atomFun id Nothing)
  where
    bracketParser baseId = do
      char '['
      annot <- myIdentifier
      char ']'
      return $ atomFun baseId . Just $ annot

termLFwhiteSpace = whiteSpaceL >> termLF

tableLF = [ [PE.Infix (whiteSpaceL >> char '/' >> return LFRight) AssocLeft ],
            [PE.Infix (whiteSpaceL >> char '\\' >> return (\a b -> LFLeft b a)) AssocRight ] ]

parserLF = buildExpressionParser tableLF termLFwhiteSpace

parserLFeof = do
   whiteSpaceL
   x <- parserLF
   whiteSpaceL
   eof
   return x

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
