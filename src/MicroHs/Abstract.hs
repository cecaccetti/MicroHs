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

-- P x y z = z x y
scP :: Exp
scP = Sc 3 (At (At X X) X) [2, 0, 1]

-- C’ x y z w = x (y w) z
scC' :: Exp
scC' = Sc 4 (At (At X (At X X)) X) [0, 1, 3, 2]

-- C’B x y z w = x z (y w)
scC'B :: Exp
scC'B = Sc 4 (At (At X X) (At X X)) [0, 2, 1, 3]

-- U x y = y x
scU :: Exp
scU = Sc 2 (At X X) [1, 0]

-- Z x y z = x y
scZ :: Exp
scZ = Sc 3 (At X X) [0, 1]

-- S’ x y z w = x (y w) (z w)
scS' :: Exp
scS' = Sc 4 (At (At X (At X X)) (At X X)) [0, 1, 3, 2, 3]

-- R x y z = y z x
scR :: Exp
scR = Sc 3 (At (At X X) X) [1, 2, 0]

-- K2 x y z = x
scK2 :: Exp
scK2 = Sc 3 X [0]

-- B’ x y z w = x y (z w)
scB' :: Exp
scB' = Sc 4 (At (At X X) (At X X)) [0, 1, 2, 3]

-- K3 x y z w = x
scK3 :: Exp
scK3 = Sc 4 X [0]

-- K4 x y z w v = x
scK4 :: Exp
scK4 = Sc 5 X [0]
--------------------

compileOpt :: Exp -> Exp
compileOpt =  etaRewrite . compileExpSc . removeSKI . opInfix
-- compileOpt = removeSKI . improveT . compileExp  . opInfix

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
  | isPrim "Z" ae = scZ
  | isPrim "S'" ae = scS'
  | isPrim "R" ae = scR
  | isPrim "K2" ae = scK2
  | isPrim "B'" ae = scB'
  | isPrim "K3" ae = scK3
  | isPrim "K4" ae = scK4
  | otherwise = ae

isOp :: Lit -> Bool
isOp (LPrim lit)
  | lit == "+" = True
  | lit == "-" = True
  | lit == "*" = True
  | lit == "==" = True
  | lit == "/=" = True
  | lit == "<" = True
  | lit == "<=" = True
  | lit == ">" = True
  | lit == ">=" = True
  | otherwise = False
isOp _ = False

opInfix :: Exp -> Exp
opInfix (App (Lit (LPrim "neg")) (Lit (LInt i))) = Lit (LInt (-i))
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
    App f a ->
      let
        fOld = case f of
                 Lam y e -> abstractSc y e
                 _ -> f
        aOld = case a of
                 Lam y e -> abstractSc y e
                 _ -> a
      in combineSc (abstractSc x f) (abstractSc x a) fOld aOld
    Lam y e -> abstractSc x $ etaReduce $ abstractSc y e
    Lit _ -> App scK ae 
    Sc ar pt is -> -- App scK ae
      if ar < 6 -- FIXME: parameterise this (maybe allow 7??)
      then Sc (ar + 1) pt (map (+ 1) is)
      else App scK ae

-- this function is deprecated
abstractCurry :: Ident -> Exp -> Exp
abstractCurry x ae =
  case ae of
    App f a ->
      let
        (c, args) = spine ae
        findIndices n list = findIndices' n list 0 []
        findIndices' _ [] _ acc = acc
        findIndices' n (x:xs) i acc
          | x == n = findIndices' n xs (i+1) (i:acc)
          | otherwise = findIndices' n xs (i+1) acc
        occurX = findIndices (Var x) args
      in
        case c of
          Sc ar p is ->
            case occurX of
              [] ->
                if ar < 6 -- FIXME: prarameterise this
                then foldl App (Sc (ar + 1) p (map (\i -> if i >= length args then i + 1 else i) is)) args
                else abstractSc x ae
              xs ->
                let
                  shouldShift n list = length $ filter (< n) list
                  adjust :: [Int] -> [Int]
                  adjust idxs =
                    let lift = map (\i -> if elem i xs then length args - length xs else i) idxs
                        shift = map (\i -> if not (elem i xs) && i < length args then i - shouldShift i xs else i) idxs
                    in map (\(origin, shifted, lifted) -> if shifted /= origin then shifted
                                                          else if lifted /= origin then lifted
                                                          else origin) (zip3 idxs shift lift)
                in foldl App (Sc (ar - length xs + 1) p (adjust is)) (filter (\e -> e /= Var x) args)
          _ -> abstractSc x ae
    _ -> ae

example = Lam (mkIdent "x") (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (App (App (Var (mkIdent "x")) (Lit (LPrim "*"))) (Var (mkIdent "x"))) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z"))))))

example1 = (Lam (mkIdent "z") (App (App (App (App (Var (mkIdent "x")) (Lit (LPrim "*"))) (Var (mkIdent "x"))) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z")))))

example2 = (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (App (App (Var (mkIdent "x")) (Lit (LPrim "*"))) (Var (mkIdent "x"))) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z"))))))

exampleSmall = Lam (mkIdent "x") (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (Var (mkIdent "x")) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z"))))))

exampleSmall' = Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (Var (mkIdent "x")) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z")))))

exampleSmall'' = Lam (mkIdent "z") (App (App (Var (mkIdent "x")) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z"))))

exampleBig = Lam (mkIdent "a") (App (Var (mkIdent "a")) (App (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (App (App (Var (mkIdent "x")) (Lit (LPrim "*"))) (Var (mkIdent "a"))) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z")))))) (Var (mkIdent "a"))))

exampleBig' = Lam (mkIdent "a") (Lam (mkIdent "x") (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (Var (mkIdent "x")) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "z")))))))

exampleBig'' = Lam (mkIdent "a") (App (Var (mkIdent "Y")) (Lam (mkIdent "x") (Lam (mkIdent "y") (Lam (mkIdent "z") (App (App (Var (mkIdent "x")) (Lit (LPrim "+"))) (App (App (Var (mkIdent "y")) (Lit (LPrim "*"))) (Var (mkIdent "a"))))))))

eqList1 = Lam (mkIdent "x2") (Lam (mkIdent "x3") (Var (mkIdent "False")))

eqList2 = App (Var (mkIdent "q2")) (App (App (Var (mkIdent "q3")) (Var (mkIdent "True"))) eqList1)

eqList3 = App (App (Var (mkIdent "eqList@")) (Var (mkIdent "x5"))) (Var (mkIdent "x7"))

eqList4 = App (Var (mkIdent "&&")) (App (App (Var (mkIdent "q1")) (Var (mkIdent "x4"))) (Var (mkIdent "x6")))

eqList5 = Lam (mkIdent "x6") (Lam (mkIdent "x7") (App eqList4 eqList3))

eqList6 = App (Var (mkIdent "q3")) (Var (mkIdent "False"))

eqList7 = Lam (mkIdent "x4") (Lam (mkIdent "x5") (App eqList6 eqList5))

eqList7' = Lam (mkIdent "x5") eqList5

eqList7'' = Lam (mkIdent "x4") (Lam (mkIdent "x5") eqList5)

eqList8 = Lam (mkIdent "eqList@") (Lam (mkIdent "q2") (Lam (mkIdent "q3") (App eqList2 eqList7)))

eqList = Lam (mkIdent "q1") (App (Var (mkIdent "Y")) eqList8)

-- ((Data.List_Type.: $x2) (NanoPrelude.takeWhile@ $x3))
takeWhile1 = App (App (Var (mkIdent "{:}")) (Var (mkIdent "x2"))) (App (Var (mkIdent "takeWhile@")) (Var (mkIdent "x3")))
-- (($q1 $x2) Data.List_Type.[])
takeWhile2 = App (App (Var (mkIdent "q1")) (Var (mkIdent "x2"))) (Var (mkIdent "[]"))
-- (\$x2. (\$x3. ((($q1 $x2) Data.List_Type.[]) ((Data.List_Type.: $x2) (NanoPrelude.takeWhile@ $x3)))))
takeWhile3 = Lam (mkIdent "x2") (Lam (mkIdent "x3") (App takeWhile2 takeWhile1))
--
takeWhile4 = Lam (mkIdent "takeWhile@") (Lam (mkIdent "q2") (App (App (Var (mkIdent "q2")) (Var (mkIdent "[]"))) takeWhile3))
takeWhile4' = Lam (mkIdent "q2") (App (App (Var (mkIdent "q2")) (Var (mkIdent "[]"))) takeWhile3)

takeWhile' = Lam (mkIdent "q1") (App (Var (mkIdent "Y")) takeWhile4)

combineSc :: Exp -> Exp -> Exp -> Exp -> Exp
combineSc a1 a2 a1Old' a2Old' =
  let
    (c1, args1) = spine a1
    (c2, args2) = spine a2
  in
    case (c1, c2) of
      (Sc ar1 p1 is1, Sc ar2 p2 is2) ->
        if a1IsUnary && a2IsUnary then -- standard combine
          if getHoles p1 + getHoles p2 <= 6 && ar1 + ar2 - 1 <= 6 then -- FIXME: parameterise this
            let
              c = Sc (ar1 + ar2 - 1) (At p1 p2) (map redirect is1 ++ map (+ length args1) is2)
              redirect i = if i == ar1 - 1 then ar1 + ar2 - 2 else i
            in foldl App c (args1 ++ args2)
          else if notElem (length args2) is2 && getHoles p1 <= 5 && ar1 <= 5 then -- x is not used on right, append left
            let
              c = Sc (ar1 + 1) (At p1 X) (map redirect is1 ++ [length args1])
              redirect i = if i == ar1 - 1 then ar1 else i
            in foldl App c (args1 ++ [etaRewrite a2Old])
          else if getHoles p1 <= 4 && ar1 <= 5 then -- 6 - 2 = 4; 6 - 1 = 5; append left
            let
              c = Sc (ar1 + 1) (At p1 (At X X)) (map redirect is1 ++ [length args1, length args1 + 1])
              redirect i = if i == ar1 - 1 then ar1 else i
            in foldl App c (args1 ++ [etaRewrite a2])
          else if notElem (length args1) is1 && getHoles p2 <= 5 && ar2 <= 5 then -- x not used on left, append on right
            let c = Sc (ar2 + 1) (At X p2) (0 : map (+ 1) is2)
            in foldl App c (etaRewrite a1Old : args2)
          else if getHoles p2 <= 4 && ar2 <= 5 then -- append right
            let
              c = Sc (ar2 + 1) (At (At X X) p2) ([0, length args2 + 1] ++ map (+ 1) is2)
            in foldl App c (etaRewrite a1 : args2)
          else
            addSc a1Old c1 args1 a2Old c2 args2
        else if a1IsUnary then -- a2 is not unary
          if notElem (length args2) is2 && getHoles p1 <= 5 && ar1 <= 5 then -- x is not used on right, append left
            let c = Sc (ar1 + 1) (At p1 X) (map redirect is1 ++ [ar1 - 1])
                redirect i = if i == ar1 - 1 then ar1 else i
            in App (foldl App c args1) (etaRewrite a2Old)
          else if getHoles p1 <= 4 && ar1 <= 5 then -- 6 - 2 = 4; 6 - 1 = 5
            let c = Sc (ar1 + 1) (At p1 (At X X)) (map redirect is1 ++ [ar1 - 1, ar1]) -- appendLeftSc
                redirect i = if i == ar1 - 1 then ar1 else i
            in foldl App c (args1 ++ [etaRewrite a2])
          -- else if notElem (length args1) is1 && getHoles p2 <= 5 && ar2 <= 5 then -- x not used on left, append on right
          --   let c = Sc (ar2 + 1) (At X p2) (0 : map (+ 1) is2)
          --   in foldl App c (a1Old : args2)
          -- else if getHoles p2 <= 4 && ar2 <= 5 then -- append on right
          --   let c = Sc (ar2 + 1) (At (At X X) p2) ([0, length args2 + 1] ++ map (+ 1) is2)
          --   in foldl App c (a1 : args2)
          else
            addSc a1Old c1 args1 a2Old c2 args2 -- consider how to compress later
        else if a2IsUnary &&
                length (filter (== length args1 + 1) is1) <= 1 && -- avoid recompute
                getHoles p1 + length (filter (== length args1 + 1) is1) * (getHoles p2 - 1) <= 6 then -- a1 is not unary, will absorb a2
          let
            updatePat p is = updatePatWith p is p2
            newAr = ar1 + ar2 - 2
            newPat = updatePat p1 is1
            redirect i 
              | i == length args1 = 42
              | i > length args1 + 1 = -i
              | otherwise = i 
            move i = if i < 0 then (-i) + length args2 - 1 else i
            c = Sc newAr newPat
                (map ((\n -> if n == 42 then length args1 + length args2 else n) . move)
                  (replaceWith (map redirect is1) (length args1 + 1) (map (+ length args1) is2)))
          in foldl App c (args1 ++ args2)
        else -- both a1 and a2 are not unary; or only a2 is unary, but cannot be absorbed
          if notElem (length args2) is2 then -- append on left (x is not used on right)
            let
              redirect i = if i == length args1 then -1 else i 
              c = Sc ar1 p1
                (map (\n -> if n == -1 then length args1 + 1 else n)
                 (replaceWith (map redirect is1) (length args1 + 1) [length args1]))
            in App (foldl App c args1) (etaRewrite a2Old)
          else if length (filter (== length args1 + 1) is1) <= 1 && -- avoid recompute
                  getHoles p1 + length (filter (== length args1 + 1) is1) <= 6 then -- append on left
            let
              updatePat p is = updatePatWith p is (At X X)
              newPat = updatePat p1 is1
              redirect i = if i == length args1 then -1 else i 
              c = Sc ar1 newPat
                (map (\n -> if n == -1 then length args1 + 1 else n)
                 (replaceWith (map redirect is1) (length args1 + 1) [length args1, length args1 + 1]))
            in App (foldl App c args1) (etaRewrite a2)
          -- else if getHoles p2 <= 4 && ar2 <= 5 then -- append on right
          --     let c = Sc (ar2 + 1) (At (At X X) p2) ([0, length args2 + 1] ++ map (+ 1) is2)
          --     in foldl App c (a1 : args2)
          else
            addSc a1Old c1 args1 a2Old c2 args2 -- consider how to compress later
        where
          a1Old = discardSc c1 args1
          a2Old = discardSc c2 args2
          a1IsUnary = ar1 == length args1 + 1
          a2IsUnary = ar2 == length args2 + 1
          -- updatePat :: Pat -> [Idx] -> Pat -> Pat
          updatePatWith X [i] p = if i == length args1 + 1 then p else X
          updatePatWith (At pt1 pt2) is p =
            let (i1, i2) = splitAt (getHoles pt1) is
            in At (updatePatWith pt1 i1 p) (updatePatWith pt2 i2 p)
          updatePatWith _ _ _ = error "strange pat"
          replaceWith :: [Int] -> Int -> [Int] -> [Int]
          replaceWith [] _ _ = []
          replaceWith (n:ns) v rpl =
            if n == v then
              rpl ++ replaceWith ns v rpl
            else
              n : replaceWith ns v rpl
      _ -> app2 scS a1 a2

addSc :: Exp -> Exp -> [Exp] -> Exp -> Exp -> [Exp] -> Exp
addSc a1Old' c1 args1 a2Old' c2 args2 =
  case (c1, c2) of
    (Sc ar1 p1 is1, Sc ar2 p2 is2) ->
      let
        a1NotUsed = notElem (length args1) is1
        a2NotUsed = notElem (length args2) is2
        a1Old = etaRewrite $ discardSc c1 args1
        a2Old = etaRewrite $ discardSc c2 args2
        a1Eta = etaRewrite $ fromSpine (c1, args1) 
        a2Eta = etaRewrite $ fromSpine (c2, args2)
      in if a1NotUsed && a2NotUsed
         then App scK (App a1Old a2Old)
         else if a1NotUsed
         then app2 scB a1Old a2Eta
         else if a2NotUsed
         then app2 scC a1Eta a2Old
         else app2 scS a1Eta a2Eta
    _ -> undefined

discardSc :: Exp -> [Exp] -> Exp
discardSc c args = -- FIXME: discarding (K e) should return (e) instead of (I e)
  case c of
    Sc ar p is -> -- should not contain any `length args` in `is`
      let
        redirect i 
          | i < length args = i
          | i > length args = i - 1
          | otherwise = error "should not discard"
        c' = Sc (ar - 1) p (map redirect is)
      in foldl App c' args
    _ -> undefined

appendLeftSc :: Exp -> Exp -> Exp
appendLeftSc a1 a2 = undefined

appendRightSc :: Exp -> Exp -> Exp
appendRightSc a1 a2 = undefined

noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = x `notElem` xs && noDuplicates xs

etaRewrite :: Exp -> Exp -- maybe this should be a fixed point function?
etaRewrite ae = 
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
        -- safeToApply = noDuplicates args
      in
        case c of
          Sc ar p is ->
            if ar == length args && safeToApply
            then fromPat p is (map etaRewrite args) 
            else if ar == length args + 1 && isOnlyLast (ar - 1) is && smallTail p && safeToApply
            then fromPat (stripTail p) (init is) args
            else fromSpine (c, map etaRewrite args)
            where
              safeToApply = noDuplicates is
          _ -> fromSpine (c, map etaRewrite args)
    _ -> ae
    
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
        else if getHoles p1 <= 4 && ar1 <= 5 -- 6 - 2 = 4; 6 - 1 = 5
        then let
          c = Sc (ar1 + 1) (At p1 (At X X)) (map redirect is1 ++ [ar1 - 1, ar1])
          redirect i = if i == ar1 - 1 then ar1 else i
          in foldl App c (args1 ++ [etaReduce a2])
        else if getHoles p2 <= 4 && ar2 <= 5
        then let
          c = Sc (ar2 + 1) (At (At X X) p2) ([0, ar2] ++ map redirect is2)
          redirect i = i + 1
          in foldl App c (etaReduce a1 : args2)
        else let
          a1NotUsed = notElem (ar1 - 1) is1
          a2NotUsed = notElem (ar2 - 1) is2
          a1Improved = discardSc c1 args1 -- $ fromPat p1 is1 args1
          a2Improved = discardSc c1 args1 -- $ fromPat p2 is2 args2
          a1Eta = etaReduce a1
          a2Eta = etaReduce a2
          in if a1NotUsed && a2NotUsed
             then App scK (App a1Improved a2Improved)
             else if a1NotUsed
             then app2 scB a1Improved a2Eta
             else if a2NotUsed
             then app2 scC a1Eta a2Improved
             else app2 scS a1Eta a2Eta
      _ -> app2 scS a1 a2

etaReduce :: Exp -> Exp -- only works for expressions that "need" x; not "discard" ones
etaReduce = id -- maybe we want to make sure, eta won't populate any App
-- etaReduce ae = 
--   case ae of
--     App f a ->
--       let
--         (c, args) = spine ae
--         isOnlyLast :: Int -> [Int] -> Bool
--         isOnlyLast x xs = last xs == x && count x xs == 1
--           where count n = length . filter (== n)
--         smallTail (At _ X) = True
--         smallTail _ = False
--         stripTail (At p X) = p
--         stripTail p = p
--       in
--         case c of
--           Sc ar p is ->
--             if ar == length args
--             then fromPat p is args -- this is harmful for let expressions
--             else if ar == length args + 1 && isOnlyLast (ar - 1) is && smallTail p
--             then fromPat (stripTail p) (init is) args
--             else fromSpine (c, map etaReduce args)
--           _ -> fromSpine (c, map etaReduce args)
--     _ -> ae

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
cS CK              _           = CI                -- S K e           = I        NOT USING THE WHOLE e!
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
