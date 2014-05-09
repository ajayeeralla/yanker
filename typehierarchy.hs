import Data.List
import Data.Monoid
import Data.Semigroup

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
data LambekFun = LFAtom String | LFLeft LambekFun LambekFun | LFRight LambekFun LambekFun
-- Pretty printing
renderLF (LFAtom s) = s
renderLF (LFLeft b a) = (renderLF b) ++ "\\" ++ (renderLF a)
renderLF (LFRight a b) = (renderLF a) ++ "/" ++ (renderLF b)

-- Lambek types, with products
data LambekType = LTAtom String | LTLeft LambekType LambekType | LTRight LambekType LambekType | LTProd LambekType LambekType

instance Semigroup LambekType where
   (<>) = LTProd