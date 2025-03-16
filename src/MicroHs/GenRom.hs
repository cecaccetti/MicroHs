module MicroHs.GenRom(genRom,killDead) where
import Prelude(); import MHSPrelude
import Data.List
import qualified MicroHs.IdentMap as M
import Data.Maybe
import MicroHs.Desugar(LDef)
import MicroHs.Exp
import MicroHs.Expr(Lit(..), showLit, errorMessage, HasLoc(..))
import MicroHs.Ident(Ident(..), showIdent, mkIdent)
import MicroHs.State

-- generate Chisel ROM file

header :: String
header = "\
 \package mutator\n\
 \import common.Helper._\n\
 \ \n"

object :: String -> (String -> String) -> (String -> String)
object name r =
  (("object " ++ name ++ " {\n") ++) . r . ("\n}" ++)

freeText :: String -> (String -> String)
freeText t = (t ++)

val :: String -> (String -> String) -> (String -> String)
val name r =
  (("val " ++ name ++ " = ") ++) . r 

-- rom
prog :: String -> (String -> String) -> (String -> String)
prog name r = val name (("Seq(\n" ++) . r . (")" ++)) 

-- top level functions
template :: (String -> String) -> (String -> String) -> (String -> String)
template comment r = ("templateBuilder( //" ++) . comment . ("\n" ++) . r . ("\n),\n" ++)

-- spine application
app :: Int -> (String -> String) -> (String -> String)
app offset r = ("appBuilder( // " ++) . (show offset ++) . ("\n" ++) . r . ("),\n" ++)

-- atoms
comb :: Int -> Pat -> [Int] -> (String -> String)
comb art p is = ("comBuilder(" ++) .
                ((show art ++ ",") ++) .
                ((show (getPatNum p) ++ ",") ++) .
                ((listPrint is ++ "),\n") ++)

fun :: Int -> (String -> String)
fun n = ("funBuilder(" ++) . (show n ++) . ("),\n" ++)

ptr :: Int -> (String -> String)
ptr n = ("ptrBuilder(" ++) . (show n ++) . ("),\n" ++)

int :: Int -> (String -> String)
int n = ("intBuilder(" ++) . (show n ++) . ("),\n" ++)

prim :: String -> (String -> String)
prim op = ("prmBuilder(\"" ++) . (op ++) . ("\"),\n" ++)

y :: (String -> String)
y = ("yBuilder(),\n" ++)

err :: Int -> (String -> String)
err code = ("errorBuilder(" ++) . (show code ++) . ("),\n" ++)

getPatNum :: Pat -> Int
getPatNum X = 0
getPatNum p =
  let
    pre = map varHole [getHoles p - 1, getHoles p - 2 .. 1]
  in idxHole p + sum pre - 1

varHole :: Int -> Int
varHole 1 = 1
varHole 2 = 1
varHole n =
  let
    pairs = [ (i, n - i) | i <- [1..n], n - i >= 1 ]
  in sum (map (\(a, b) -> varHole a * varHole b) pairs)

idxSplit :: Pat -> Int
idxSplit X = 1
idxSplit (At a b) = (idxHole a - 1) * varHole (getHoles b) + idxHole b

idxHole :: Pat -> Int
idxHole X = 1
idxHole (At a b) =
  let
    ah = getHoles a
    bh = getHoles b
    pairs = [ (i, ah + bh - i) | i <- [ah+1..ah+bh], ah + bh - i >= 1]
  in idxSplit (At a b) + foldr (+) 0 (map (\(a, b) -> varHole a * varHole b) pairs)

listPrint :: [Int] -> String
listPrint [] = "List()"
listPrint xs = "List(" ++ inner ++ ")"
  where
    inner = concat $ zipWith (\x y -> show x ++ y) xs (replicate (length xs - 1) ", " ++ [""])
    
killDead :: (Ident, [LDef]) -> [LDef]
killDead (mainName, ds) =
  let
    dMap = M.fromList ds
    -- Shake the tree bottom-up, serializing nodes as we see them.
    -- This is much faster than (say) computing the sccs and walking that.
    dfs :: Ident -> State (Int, M.Map Exp, [LDef]) ()
    dfs n = do
      (i, seen, r) <- get
      case M.lookup n seen of
        Just _ -> return ()
        Nothing -> do
          -- Put placeholder for n in seen.
          put (i, M.insert n (Var n) seen, r)
          -- Walk n's children
          let e = findIdentIn n dMap
          mapM_ dfs $ freeVars e
          -- Now that n's children are done, compute its actual entry.
          (i', seen', r') <- get
          put (i'+1, M.insert n (ref i') seen', (n, e) : r')
    (_,(_, _, res)) = runState (dfs mainName) (0, M.empty, [])
    
    ref i = Var $ mkIdent $ "FUN" ++ show i
    findIdentIn n m = fromMaybe (errorMessage (getSLoc n) $ "AA No definition found for: " ++ showIdent n) $
                      M.lookup n m
  in res    

genRom :: (Ident, [LDef]) -> String
genRom (mainName, ldefs) =
  let
    ds = inlineSingle ldefs
    dMap = M.fromList ds
    -- state: 1. fun counter; 2. app counter; 3. comb counter; 4. function map; 5. resulting string
    dfs :: Ident -> State (Int, Int, Int, M.Map Exp, String -> String) ()
    dfs n = do
      (i, ptr, combs, seen, r) <- get
      case M.lookup n seen of
        Just _ -> return ()
        Nothing -> do
          let e = findIdentIn n dMap
          put (i, ptr + 1, combs, M.insert n (ref ptr) seen, r)
          -- print this function
          buildFunc (substv e) (showIdent n)
          (i', ptr', combs', seen', r') <- get
          put (i + 1, ptr', combs', seen', r')
          -- Walk n's children
          mapM_ dfs $ freeVars e

    buildFunc :: Exp -> String -> State (Int, Int, Int, M.Map Exp, String -> String) ()
    buildFunc e name =
      let
        -- state: 1. ptr counter; 2. comb counter; 3. current spine; 4. apps
        build :: Exp -> State (Int, Int, String -> String, [String -> String]) ()
        build e = do
          (i, combs, s, as) <- get
          case e of
            App f (App a1 a2) -> do
              put (i, combs, freeText "", as)
              build (App a1 a2)
              (i', combs', s', as') <- get
              put (i' + 1, combs', ptr i' . s, as' ++ [app i' s'])
              build f
            App f a -> do
              let combs' =
                    case a of
                      Sc _ _ _ -> combs + 1
                      _ -> combs
              put(i, combs', atom a . s, as)
              build f
            _ -> do
              let combs' =
                    case e of
                      Sc _ _ _ -> combs + 1
                      _ -> combs
              put(i, combs', atom e . s, as)
      in do
      (i, ptr, combs, seen, r) <- get
      let (_, (ptr', combs', spn, aps)) = runState (build e) (ptr, combs, freeText "", [])
      put (i, ptr', combs', seen, r . ((" // FUN" ++ show i ++ name  ++ "\n") ++) . app (ptr - 1) spn . foldr (.) (freeText "") aps)
           
          
    (_, (funCount, appCount, combCount, defs, res)) = runState (dfs mainName) (0, 0, 0, M.empty, freeText "")
    ref i = Var $ mkIdent $ "PTR" ++ show i
    findIdentIn n m = fromMaybe (errorMessage (getSLoc n) $ "No definition found for: " ++ showIdent n) $
                      M.lookup n m
    findIdent n = findIdentIn n defs
    substv aexp =
      case aexp of
        Var n -> findIdent n
        App f a -> App (substv f) (substv a)
        e -> e
  in header ++
     "// Functions in this file: " ++ show funCount ++ "\n"
     ++ "// Apps in this file: " ++ show appCount ++ "\n"
     ++ "// Combinators in this file: " ++ show combCount ++ "\n"
     ++ object "ProgramBin" (prog "prog" res) ""

atom :: Exp -> (String -> String)
atom ae =
  case ae of
    Var i -> if "PTR" `isPrefixOf` (showIdent i) then ptr $ read (drop 3 (showIdent i))
               else error "Strange Var exists."
    Lit (LInt i) -> int i
    Lit (LPrim "Y") -> y
    Lit (LPrim ('e':'r':'r':'o':'r':rest)) -> err $ read rest
    Lit (LPrim op) -> if "error" `isPrefixOf` op then err $ read (drop 5 op)
                        else prim op
    Lit _ -> error "Strange Lit exists."
    Sc a p is -> comb a p is
    _ -> error "Not an Atom."

inlineSingle :: [LDef] -> [LDef]
inlineSingle defs =
  let
    isSingle (App _ _) = False
    isSingle _ = True
    sub :: (Ident, Exp) -> Exp -> Exp
    sub (i, single) (Var i') = if i == i' then single else Var i'
    sub (i, single) (App f a) = App (sub (i, single) f) (sub (i, single) a)
    sub _ ae = ae
    subSingle :: (Ident, Exp) -> [(Ident, Exp)] -> [(Ident, Exp)]
    subSingle (i, single) = map (\(i', e) -> (i', sub (i, single) e))
    inlineSingle' :: [(Ident, Exp)] -> State [(Ident, Exp)] ()
    inlineSingle' [] = return ()
    inlineSingle' ((i, e) : ies) = 
      if isSingle e then do
        s <- get
        put $ subSingle (i, e) s
        inlineSingle' ies
      else
        inlineSingle' ies
        
    (_, res) = runState (inlineSingle' defs) defs
  in res
