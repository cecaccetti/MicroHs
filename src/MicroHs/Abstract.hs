module MicroHs.Abstract(
  compileOpt,
  -- reduce,
  ) where
import Prelude(); import MHSPrelude
import MicroHs.Ident
import MicroHs.Exp
import MicroHs.Expr(Lit(..))

--
-- Used combinators
--   * indicates that the implementation uses an indirection
--   A indicates allocation in the implementation
-- S  x y z   = x z (y z)             A
-- K  x y     = x                     *
-- I  x       = x                     *
-- B  x y z   = x (y z)               A
-- C  x y z   = x z y                 A
-- S' x y z w = x (y w) (z w)         A
-- B' x y z w = x y (z w)             A
-- C' x y z w = x (y w) z             A
-- A  x y     = y                     *
-- U  x y     = y x
-- n@(Y x)    = x n
-- Z  x y z   = x y
-- P  x y z   = z x y                 A
-- R  x y z   = y z x                 A
-- O  x y z w = w x y                 A
-- K2 x y z   = x                     *
-- K3 x y z w = x                     *
-- K4 x y z w v = x                   *
-- C'B x y z w = x z (y w)            A
--

data MaybeApp = NotApp | IsApp Exp Exp

getApp :: Exp -> MaybeApp
getApp ae =
  case ae of
    App f a -> IsApp f a
    _       -> NotApp

isPrim :: String -> Exp -> Bool
isPrim s ae =
  case ae of
    Lit (LPrim ss) -> s == ss
    _       -> False

isK :: Exp -> Bool
isK = isPrim "K"

isI :: Exp -> Bool
isI = isPrim "I"

isB :: Exp -> Bool
isB = isPrim "B"

isC :: Exp -> Bool
isC = isPrim "C"

isCC :: Exp -> Bool
isCC = isPrim "C'"

isY :: Exp -> Bool
isY = isPrim "Y"

isP :: Exp -> Bool
isP = isPrim "P"

isZ :: Exp -> Bool
isZ = isPrim "Z"

isK2 :: Exp -> Bool
isK2 = isPrim "K2"

isK3 :: Exp -> Bool
isK3 = isPrim "K3"

cId :: Exp
cId = Lit (LPrim "I")

cConst :: Exp
cConst = Lit (LPrim "K")

cSpread :: Exp
cSpread = Lit (LPrim "S")

cP :: Exp
cP = Lit (LPrim "P")

scI :: Exp
scI = Sc 1 X [0]

scK :: Exp
scK = Sc 2 X [0]

scB :: Exp
scB = Sc 3 (At X (At X X)) [0, 1, 2]

scS :: Exp
scS = Sc 3 (At (At X X) (At X X)) [0, 2, 1, 2]

scC :: Exp
scC = Sc 3 (At (At X X) X) [0, 2, 1]

scA :: Exp
scA = Sc 2 X [1]

scO :: Exp
scO = Sc 4 (At (At X X) X) [3, 0, 1]

-- Pxyz zxy
scP :: Exp
scP = Sc 3 (At (At X X) X) [2, 0, 1]

-- C’ x y z w  x (y w) z
scC' :: Exp
scC' = Sc 4 (At (At X (At X X)) X) [0, 1, 3, 2]

-- C’B x y z w   x z (y w)
scC'B :: Exp
scC'B = Sc 4 (At (At X X) (At X X)) [0, 2, 1, 3]

-- Uxy yx
scU :: Exp
scU = Sc 2 (At X X) [1, 0]
--------------------

compileOpt :: Exp -> Exp
compileOpt =  etaReduce . compileExpSc . removeSKI . opInfix
-- compileOpt = removeSKI . improveT . compileExp . opInfix

removeSKI :: Exp -> Exp
removeSKI (App f a) = App (removeSKI f) (removeSKI a)
removeSKI (Lam x a) = Lam x (removeSKI a)
removeSKI ae
  | isPrim "I" ae = scI
  | isPrim "K" ae = scK
  | isPrim "B" ae = scB
  | isPrim "S" ae = scS
  | isPrim "C" ae = scC
  | isPrim "A" ae = scA
  | isPrim "O" ae = scO
  | isPrim "P" ae = scP
  | isPrim "C'" ae = scC'
  | isPrim "C'B" ae = scC'B
  | isPrim "U" ae = scU
  -- some more...
  | otherwise = ae

isOp :: Lit -> Bool
isOp (LPrim lit)
  | lit == "==" = True
  | lit == "+" = True
  | lit == "-" = True
  | lit == "*" = True
  | otherwise = False
isOp _ = False

opInfix :: Exp -> Exp
opInfix (App (App (Lit l) i1) i2)
  | isOp l = App (App (opInfix i1) (Lit l)) (opInfix i2)
  | otherwise = App (opInfix (App (Lit l) i1)) (opInfix i2)
opInfix (App f a) = App (opInfix f) (opInfix a)
opInfix (Lam x a) = Lam x (opInfix a)
opInfix ae = ae

compileExpSc :: Exp -> Exp
compileExpSc ae =
  case ae of
    App f a -> App (compileExpSc f) (compileExpSc a)
    Lam x a -> abstractSc x a
    _       -> ae

abstractSc :: Ident -> Exp -> Exp
abstractSc x ae =
  case ae of
    Var y -> if x == y then scI else App scK (Var y)
    App f a -> scCombine (abstractSc x f) (abstractSc x a)
    --Lam y e -> abstractCurry x $ etaReduce $ abstractSc y e
    -- Lam y e -> abstractCurry x $ abstractSc y e
    Lam y e -> abstractSc x $ etaReduce $ abstractSc y e
    Lit _ -> App scK ae -- Y combinator can make things ugly
    Sc _ _ _ -> App scK ae
      -- if ar < 6 -- FIXME: parameterise this
      -- then App scK ae--Sc (ar + 1) pt (map (+ 1) is)
      -- else App scK ae -- fix this for curry

abstractCurry :: Ident -> Exp -> Exp -- FIXME: not working for more than two arguments
abstractCurry x ae =
  case ae of
    Var y -> ae--if x == y then scI else App scK (Var y)
    App f a ->
      let
        (c, args) = spine ae
        findIndices n list = findIndices' n list 0 []
        findIndices' _ [] _ acc = acc
        findIndices' n (x:xs) i acc
          | x == n = findIndices' n xs (i+1) (i:acc)
          | otherwise = findIndices' n xs (i+1) acc
        occurX = findIndices (Var x) args
        shouldShift n list = length $ filter (< n) list
      in
        case c of
          Sc ar p is ->
            if True
            then case occurX of
              [] ->
                if ar < 6 -- FIXME: prarameterise this
                then foldl App (Sc (ar + 1) p (map (\i -> if i >= length args then i + 1 else i) is)) (map (abstractCurry x) args)
                else abstractSc x ae
              xs ->
                let
                  adjust :: [Int] -> [Int]
                  adjust idxs =
                    let lift = map (\i -> if elem i xs then length args - length xs else i) idxs
                        shift = map (\i -> if (not (elem i xs)) && i < length args then i - shouldShift i xs else i) idxs
                    in map (\(origin, shifted, lifted) -> if shifted /= origin then shifted
                                                          else if lifted /= origin then lifted
                                                          else origin) (zip3 idxs shift lift)
                in foldl App (Sc (ar - length xs + 1) p (adjust is)) (map (abstractCurry x) (filter (\e -> e /= Var x) args)) --(map (abstractCurry x ) args)
            else abstractSc x ae
          _ -> abstractSc x ae
    Lam _ _ -> undefined
    Lit _ -> ae--App scK ae
    Sc _ _ _ -> ae--App scK ae

example :: Exp
example = Lam (mkIdent "x") (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (App (App (Var (mkIdent "x")) (Lit (LPrim "*"))) (Var (mkIdent "x"))) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z"))))))

example1 = (Lam (mkIdent "z") (App (App (App (App (Var (mkIdent "x")) (Lit (LPrim "*"))) (Var (mkIdent "x"))) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z")))))

example2 = (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (App (App (Var (mkIdent "x")) (Lit (LPrim "*"))) (Var (mkIdent "x"))) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z"))))))

exampleSmall = Lam (mkIdent "x") (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (Var (mkIdent "x")) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z"))))))

    
scCombine' a1 a2 =
  let
    (c1, args1) = spine a1
    (c2, args2) = spine a2
  in
    case (c1, c2) of -- (a1, a2) has no problem
      (Sc ar1 p1 is1, Sc ar2 p2 is2) -> app2 scS a1 a2
      _ -> app2 scS a1 a2
    
-- this rule always assume a1 and a2 are "unary"
scCombine :: Exp -> Exp -> Exp
scCombine a1 a2 =
  let
    (c1, args1) = spine a1
    (c2, args2) = spine a2
    args = args1 ++ args2
  in
    case (c1, c2) of
      (Sc ar1 p1 is1, Sc ar2 p2 is2) ->
        if getHoles p1 + getHoles p2 <= 6 && ar1 + ar2 - 1 <= 6 -- FIXME: parameterise this
        then let
          c = Sc (ar1 + ar2 - 1) (At p1 p2) (map redirect is1 ++ map (+ (ar1 - 1)) is2)
          redirect i = if i == ar1 - 1 then ar1 + ar2 - 2 else i
          in foldl App c args   
        else let
          a1NotUsed = notElem (ar1 - 1) is1
          a2NotUsed = notElem (ar2 - 1) is2
          a1Improved = etaReduce $ fromPat p1 is1 args1
          a2Improved = etaReduce $ fromPat p2 is2 args2
          a1Eta = etaReduce a1
          a2Eta = etaReduce a2
          in if a1NotUsed && a2NotUsed
             then App scK (App a1Improved a2Improved)
             else if a1NotUsed
             then app2 scB a1Improved a2Eta
             else if a2NotUsed
             then app2 scC a1Eta a2Improved
             else app2 scS a1Eta a2Eta
          --app2 scS a1 a2 -- do it smarter, with S/B/C
      _ -> app2 scS a1 a2

etaReduce :: Exp -> Exp
etaReduce ae = 
  case ae of
    App f a ->
      let
        (c, args) = spine ae
        isOnlyLast :: Int -> [Int] -> Bool
        isOnlyLast x xs = last xs == x && count x xs == 1
          where count n = length . filter (== n)
        smallTail (At _ X) = True
        smallTail _ = False
        stripTail (At p X) = p
        stripTail p = p
      in
        case c of
          Sc ar p is ->
            if ar == length args
            then fromPat p is args
            else if ar == length args + 1 && isOnlyLast (ar - 1) is && smallTail p
            then fromPat (stripTail p) (init is) args
            else fromSpine (c, map etaReduce args)
          _ -> fromSpine (c, map etaReduce args)
    _ -> ae

compileExp :: Exp -> Exp
compileExp ae =
  case ae of
    App f a -> App (compileExp f) (compileExp a)
    Lam x a -> abstract x a
    _       -> ae

abstract :: Ident -> Exp -> Exp
abstract x ae =
  case ae of
    Var y  -> if x == y then cId else cK (Var y)
    App f a -> cS (abstract x f) (abstract x a)
    Lam y e -> abstract x $ abstract y e
    Lit _ -> cK ae
    Sc _ _ _ -> cK ae

cK :: Exp -> Exp
cK e  = App cConst e

cS :: Exp -> Exp -> Exp
cS a1 a2 =
 if isK a1 then cId else
  let
    r = cS2 a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp k1 e1 ->
        if isK k1 then
          case getApp a2 of
            IsApp k2 e2 ->
              if isK k2 then
                cK (App e1 e2)
              else
                cB e1 a2
            NotApp ->
              if isI a2 then
                e1
              else
                cB e1 a2
        else
          r
cS2 :: Exp -> Exp -> Exp
cS2 a1 a2 =
  case getApp a2 of
    NotApp -> cS3 a1 a2
    IsApp k2 e2 ->
      if isK k2 then
        cC a1 e2
      else
        cS3 a1 a2

cS3 :: Exp -> Exp -> Exp
cS3 a1 a2 =
  let
    r = app2 cSpread a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp be1 e2 ->
        case getApp be1 of
          NotApp -> r
          IsApp b1 e1 ->
            if isB b1 then
              cSS e1 e2 a2
            else
              r

{-
--cS e1 e2 | trace ("S (" ++ toString e1 ++ ") (" ++ toString e2 ++ ")") False = undefined
cS CK              _           = CI                -- S K e           = I
cS (App CK e1)     (App CK e2) = cK (App e1 e2)    -- S (K e1) (K e2) = K (e1 e2)
cS (App CK e1)     CI          = e1                -- S (K e1) I      = e1
cS (App CK e1)     e2          = cB e1 e2          -- S (K e1) e2     = B e1 e2
cS e1              (App CK e2) = cC e1 e2          -- S e1     (K e2) = C e1 e2
cS (App (App CB e1) e2) e3     = cSS e1 e2 e3      -- S (B e1 e2) e3  = S' e1 e2 e3
cS e1 e2                       = App2 CS e1 e2
-}

cC :: Exp -> Exp -> Exp
cC a1 e3 =
  let
    r = cC2 a1 e3
  in
    case getApp a1 of
      NotApp -> r
      IsApp x1 e2 ->
        case getApp x1 of
          NotApp -> r
          IsApp bc e1 ->
            if isB bc then
              cCC e1 e2 e3
            else if isC bc && isI e1 then
              app2 cP e2 e3
--            else if isC bc && isC e1 then
--              app2 cR e2 e3
            else
              r

cC2 :: Exp -> Exp -> Exp
cC2 a1 a2 = app2 cFlip a1 a2

{-
cC (App (App CB e1) e2) e3          = cCC e1 e2 e3      -- C (B e1 e2) e3  = C' e1 e2 e3
cC (Var op)             e2 | Just op' <- lookup op flipOps = App (Var op') e2 -- C op e = flip-op e
cC (App (App CC CI) e2) e3          = app2 CP e2 e3
cC (App (App CC CC) e2) e3          = app2 CR e2 e3
cC e1                   e2          = app2 CC e1 e2
-}

cB :: Exp -> Exp -> Exp
cB a1 a2 =
  let
    r = cB2 a1 a2
  in
    case getApp a1 of
      NotApp -> r
      IsApp cb ck ->
        if isB cb && isK ck && isP a2 then
          Lit (LPrim "O")
        else
          r

cB2 :: Exp -> Exp -> Exp
cB2 a1 a2 =
  let
    r = cB3 a1 a2
  in
    case getApp a2 of
      IsApp x1 x2 ->
        case getApp x1 of
          IsApp cb ck ->
            if isY a1 && isB cb && isK ck then
              x2
            else
              r
          NotApp ->
            if isC a1 && isC x1 && isI x2 then
              cP
            else
              r
      NotApp -> r

cB3 :: Exp -> Exp -> Exp
cB3 a1 a2 =
  if isI a1 then
    a2
  else
    app2 (Lit (LPrim "B")) a1 a2

{-
cB (App CB CK) CP             = CO -- Cons
cB CY          (App (App CB CK) e) = e  -- B Y (B K e) = e
cB CC          (App CC CI)    = CP -- Pair
cB CI          e              = e  -- B I e = e
cB e1          e2             = app2 CB e1 e2
-}

cSS :: Exp -> Exp -> Exp -> Exp
cSS e1 e2 e3 = app3 (Lit (LPrim "S'")) e1 e2 e3

cCC :: Exp -> Exp -> Exp -> Exp
cCC e1 e2 e3 = app3 (Lit (LPrim "C'")) e1 e2 e3

improveT :: Exp -> Exp
improveT ae =
  case getApp ae of
    NotApp -> ae
    IsApp f a ->
      let
        ff = improveT f
        aa = improveT a
      in
        if isK ff && isI aa then
          Lit (LPrim "A")
{- Using I x --> x does not improve things.
        else if isI ff then
          aa
-}
        else if isB ff && isK aa then
          Lit (LPrim "Z")
        else if isC ff && isI aa then
          Lit (LPrim "U")
        else if isB ff && isB aa then
          Lit (LPrim "B'")
        else if isC ff && isC aa then
          Lit (LPrim "R")
        else if isCC ff && isB aa then
          Lit (LPrim "C'B")
        else if isZ ff && isK aa then
          Lit (LPrim "K2")
        else if isZ ff && isK2 aa then
          Lit (LPrim "K3")
        else if isZ ff && isK3 aa then
          Lit (LPrim "K4")
        else
          let
            def =
              case getApp aa of
                IsApp ck e ->
                  if isY ff && isK ck then
                    e
                  else
                    kApp ff aa
                NotApp -> kApp ff aa
          in
            def
{-
            case getApp ff of
              IsApp xf xa ->
                if isK xf then
                  xa
                else
                  def
              NotApp -> def
-}
            

kApp :: Exp -> Exp -> Exp
kApp (Lit (LPrim "K")) (App (Lit (LPrim ('K':s))) x)
  | s == ""  = App (Lit (LPrim "K2")) x
  | s == "2" = App (Lit (LPrim "K3")) x
  | s == "3" = App (Lit (LPrim "K4")) x
kApp f a = App f a

{-
-- K I      -->  A
-- C I      -->  T
-- B B      -->  B'
-- Y (K e)  -->  e
-- K x y    -->  x
improveT (App f a) =
  case (improveT f, improveT a) of
    (CK,                     CI) -> CA
--    (CI,                      e) -> e
    (CY,               App CK e) -> e
--    (App CK e1,              e2) -> e1
    (e1,                     e2) -> App e1 e2
improveT e = e
-}

--------
-- Possible additions
--
-- Added:
--  R = C C
--  R x y z = (C C x y) z = C y x z = y z x
--
--  Q = C I
--  Q x y z = (C I x y) z = I y x z = y x z
--
-- Added:
--  Z = B K
--  Z x y z = B K x y z = K (x y) z = x y
--
--  ZK = Z K
--  ZK x y z = Z K x y z = (K x) z = x
--
--  C'B = C' B
--  C'B x y z w = C' B x y z w = B (x z) y w = x z (y w)

--  B (B e) x y z = B e (x y) z = e (x y z)
--
--  B' :: (a -> b -> c) -> a -> (d -> b) -> d -> c
--  B' k f g x = k f (g x)
--
-- Common:
--  817: C' B
--  616: B Z
--  531: C' C
--  352: Z K
--  305: C' S
--
--  BZ = B Z
--  BZ x y z w = B Z x y z w = Z (x y) z w = x y z
--
--  C'C = C' C
--  C'C x y z w = C' C x y z w = C (x z) y w = x z w y
--
--  C'B P x y z w = P y (x z) w = w y (x z)

{- This makes very little difference.
   There are only 2 reductions in the entire compiler.
reduce :: Exp -> Exp
reduce e = red e []
  where
    -- No duplication, nor cycles, so no S, S', Y
    red (App f a) as                      = red f (reduce a : as)
    red f (x:as)         | isI          f && xxx "I" = red x as
    red f (x:y:as)       | isPrim "A"   f && xxx "A" = red y as
                         | isPrim "U"   f && xxx "U" = red y (x : as)
                         | isK          f && xxx "K" = red x as
    red f (x:y:z:as)     | isB          f && xxx "B" = red x (App y z : as)
                         | isC          f && xxx "C" = red x (z : y : as)
                         | isPrim "Z"   f && xxx "Z" = red x (y : as)
                         | isP          f && xxx "P" = red z (x : y : as)
                         | isPrim "R"   f && xxx "R" = red y (z : x : as)
                         | isPrim "K2"  f && xxx "K2" = red x as
    red f (x:y:z:w:as)   | isPrim "B'"  f && xxx "B'" = red x (y : App z w : as)
                         | isPrim "C'"  f && xxx "C'" = red x (App y w : z : as)
                         | isPrim "O"   f && xxx "O" = red w (x : y : as)
                         | isPrim "K3"  f && xxx "K3" = red x as
                         | isPrim "C'B" f && xxx "C'B" = red x (z : App y w : as)
    red f (x:_:_:_:_:as) | isPrim "K4"  f && xxx "K4" = red x as

    red f as                              = foldl App f as

    xxx s = trace s True
-}
