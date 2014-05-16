module TypeHierarchy where

import Data.List
import Data.Functor
import Data.Monoid

import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language

import qualified Data.Map.Strict as Map
-- import Data.Semigroup

-- Simple type in a pregroup: a base type and an exponent. +1 means right, -1 means left
data PrgSType = PrgS String Int
        deriving (Show, Eq)

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
   deriving (Eq,Show,Ord)

-- Pretty printing
data ParenthesisNeeded = NoParen | AlwaysParen | ParenLeft | ParenRight
addParen str = "("++str++")"

renderLFparen (LFAtom s Nothing) _ = s
renderLFparen (LFAtom s (Just annot)) _ = s ++ "[" ++ annot ++ "]"

renderLFparen (LFLeft b a) NoParen =
  (renderLFparen b AlwaysParen) ++ "\\" ++ (renderLFparen a ParenRight)
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
    | LSVar String
    | LSLeft LambekSkel LambekSkel
    | LSRight LambekSkel LambekSkel
   deriving (Eq,Show,Ord)

-- Pretty printing
renderLS (LSAtom s Nothing) = s
renderLS (LSAtom s (Just annot)) = s ++ "[" ++ annot ++ "]"
renderLS (LSVar s) = s -- TODO: find a way to distinguish between the two?
renderLS (LSLeft a b) = (renderLS b) ++ "\\" ++ (renderLS a)
renderLS (LSRight a b) = (renderLS a) ++ "/" ++ (renderLS b)

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

-- Lambek types, with products
data LambekType = LTAtom String | LTLeft LambekType LambekType | LTRight LambekType LambekType | LTProd LambekType LambekType

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

tableLF = [ [Infix (whiteSpaceL >> char '/' >> return LFRight) AssocLeft ],
            [Infix (whiteSpaceL >> char '\\' >> return LFLeft) AssocRight ] ]

parserLF = buildExpressionParser tableLF termLFwhiteSpace

parserLFeof = do
   whiteSpaceL
   x <- parserLF
   whiteSpaceL
   eof
   return x

---- Parsing for skeletons ----

termLS :: Parser LambekSkel
termLS = (atomLF LSAtom) <|> (P.parens lexerL parserLS)

termLSwhiteSpace = whiteSpaceL >> termLS

tableLS = [ [Infix (whiteSpaceL >> char '/' >> return LSRight) AssocLeft ],
            [Infix (whiteSpaceL >> char '\\' >> return LSLeft) AssocRight ] ]

parserLS = buildExpressionParser tableLS termLSwhiteSpace

parserLSeof = do
  whiteSpaceL
  x <- parserLS
  whiteSpaceL
  eof
  return x
