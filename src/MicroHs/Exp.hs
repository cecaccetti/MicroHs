{-# OPTIONS_GHC -Wno-unused-imports #-}
-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.Exp(
  Exp(..),
  PrimOp,
  Pat(..),
  substExp,
  app2, app3, cFlip,
  allVarsExp, freeVars,
  lams, apps, spine,
  getHoles, fromSpine, fromPat,
  ) where
import Prelude(); import MHSPrelude hiding((<>))
import Data.Char
import Data.List
import MicroHs.Ident
import MicroHs.Expr(Lit(..), showLit)
import MicroHs.List
import Text.PrettyPrint.HughesPJLite
import Debug.Trace

type PrimOp = String
type Arity = Int
type Idx = Int

data Pat = X | At Pat Pat
  deriving (Eq)

getHoles :: Pat -> Int
getHoles X = 1
getHoles (At a b) = getHoles a + getHoles b

instance Show Pat where
  show X = "X"
  show (At p (At p1 p2)) = show p ++ "(" ++ show (At p1 p2) ++ ")"
  --show (At (At p1 p2) p) = show p1 ++ show p2 ++ show p
  show (At p1 p2) = show p1 ++ show p2

data Exp
  = Var Ident
  | App Exp Exp
  | Lam Ident Exp
  | Lit Lit
  | Sc Arity Pat [Idx]
  deriving (Eq)

app2 :: Exp -> Exp -> Exp -> Exp
app2 f a1 a2 = App (App f a1) a2

app3 :: Exp -> Exp -> Exp -> Exp -> Exp
app3 f a1 a2 a3 = App (app2 f a1 a2) a3

cFlip :: Exp
cFlip = Lit (LPrim "C")

--cR :: Exp
--cR = Lit (LPrim "R")

instance Show Exp where
  show = render . ppExp

ppExp :: Exp -> Doc
ppExp ae =
  case ae of
--    Let i e b -> sep [ text "let" <+> ppIdent i <+> text "=" <+> ppExp e, text "in" <+> ppExp b ]
    Var i -> ppIdent i
    App f a -> parens $ ppExp f <+> ppExp a
    Lam i e -> parens $ text "\\" <> ppIdent i <> text "." <+> ppExp e
    Lit l -> text (showLit l)
    Sc a p is -> text $ "<" ++ show a ++ ","
                     ++ show p ++ ","
                     ++ show is
                     ++ ">"

substExp :: Ident -> Exp -> Exp -> Exp
substExp si se ae =
  case ae of
    Var i -> if i == si then se else ae
    App f a -> App (substExp si se f) (substExp si se a)
    Lam i e -> if si == i then
                 ae
               else if elem i (freeVars se) then
                 let
                   fe = allVarsExp e
                   ase = allVarsExp se
                   j = head [ v | n <- enumFrom (0::Int),
                              let { v = mkIdent ("a" ++ show n) },
                              not (elem v ase), not (elem v fe), v /= si ]
                 in
                   --trace ("substExp " ++ show [si, i, j]) $
                   Lam j (substExp si se (substExp i (Var j) e))
               else
                   Lam i (substExp si se e)
    Lit _ -> ae
    Sc _ _ _ -> ae

-- This naive freeVars seems to be the fastest.
freeVars :: Exp -> [Ident]
freeVars ae =
  case ae of
    Var i -> [i]
    App f a -> freeVars f ++ freeVars a
    Lam i e -> deleteAllBy (==) i (freeVars e)
    Lit _ -> []
    Sc _ _ _ -> []

allVarsExp :: Exp -> [Ident]
allVarsExp ae =
  case ae of
    Var i -> [i]
    App f a -> allVarsExp f ++ allVarsExp a
    Lam i e -> i : allVarsExp e
    Lit _ -> []
    Sc _ _ _ -> []

lams :: [Ident] -> Exp -> Exp
lams xs e = foldr Lam e xs

apps :: Exp -> [Exp] -> Exp
apps f = foldl App f

-- the spine of an Exp
spine :: Exp -> (Exp, [Exp])
spine ae = spine' ae []
  where
    spine' e acc =
      case e of
        App f a -> spine' f (a : acc)
        _ -> (e, acc)

fromSpine :: (Exp, [Exp]) -> Exp
fromSpine (f, []) = f
fromSpine (f, e : es) = fromSpine (App f e, es)

fromPat :: Pat -> [Idx] -> [Exp] -> Exp
fromPat X [i] es = es !! i
fromPat (At p1 p2) is es =
  let
    (is1, is2) = splitAt (getHoles p1) is
  in
    App (fromPat p1 is1 es) (fromPat p2 is2 es)
fromPat _ _ _ = error "pattern holes and index list mismatch."
