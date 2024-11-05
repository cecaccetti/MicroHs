module MicroHs.GenRom(genRom, getHoles) where
import Prelude(); import MHSPrelude
import Data.Char(ord, chr)
import Data.List
import qualified MicroHs.IdentMap as M
import Data.Maybe
import MicroHs.Desugar(LDef)
import MicroHs.EncodeData(encList)
import MicroHs.Exp
import MicroHs.Expr(Lit(..), showLit, errorMessage, HasLoc(..))
import MicroHs.Ident(Ident(..), showIdent, mkIdent)
import MicroHs.State

-- generate Chisel ROM file

header :: String
header = "\
 \package mutator\n\
 \import chisel3._\n\
 \import chisel3.util._\n\
 \import common._\n\
 \import common.SystemConfig._\n\
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
template :: (String -> String) -> (String -> String)
template r = ("templateBuilder(\n" ++) . r . ("\n),\n" ++)

-- spine application
app :: (String -> String) -> (String -> String)
app r = ("appBuilder(\n" ++) . r . ("),\n" ++)

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

getHoles :: Pat -> Int
getHoles X = 1
getHoles (At a b) = getHoles a + getHoles b

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

example :: String -> String
example = prog "example" $ template $ app $ comb 2 X [2,0,1] . int 13

genRom :: (Ident, [LDef]) -> String
genRom (mainName, ds) =
  let
    dMap = M.fromList ds
    dfs :: Ident -> State (Int, M.Map Exp, String -> String) ()
    dfs n = do
      (i, seen, r) <- get
      case M.lookup n seen of
        Just _ -> return ()
        Nothing -> do
          -- Put placeholder for n in seen.
          put (i + 1, M.insert n (Var n) seen, r)
          -- Walk n's children
          let e = findIdentIn n dMap
          mapM_ dfs $ freeVars e
          -- Now that n's children are done, compute its actual entry.
          (i', seen', r') <- get
          put (i', M.insert n (ref i) seen', def r' e)
          
    (_, (_, defs, res)) = runState (dfs mainName) (0, M.empty, freeText "")
    ref i = Var $ mkIdent $ "FUN" ++ show i
    findIdentIn n m = fromMaybe (errorMessage (getSLoc n) $ "No definition found for: " ++ showIdent n) $
                      M.lookup n m
    findIdent n = findIdentIn n defs
    substv aexp =
      case aexp of
        Var n -> findIdent n
        App f a -> App (substv f) (substv a)
        e -> e
    def :: (String -> String) -> Exp -> (String -> String)
    def r e = buildTemplate (substv e) . r 
  in header ++ object "ProgramBin" (prog "prog" res) ""

buildTemplate :: Exp -> (String -> String)
buildTemplate ae =
  let
    -- state: 1. ptr counter; 2. current spine; 3. apps
    build :: Exp -> State (Int, String -> String, [String -> String]) ()
    build e = do
      (i, s, as) <- get
      case e of
        App f (App a1 a2) -> do
          put (i, freeText "", as)
          build (App a1 a2)
          (i', s', as') <- get
          put (i' + 1, ptr i' . s, as' ++ [s'])
          build f
        App f a -> do
          put(i, atom a . s, as)
          build f
        _ -> put(i, atom e . s, as)

    (_, (_, spn, aps)) = runState (build ae) (0, freeText "", [])
  in template $ app spn . (show (length aps) ++) . (",\n" ++) . foldr ((.) . app) (freeText "") aps

atom :: Exp -> (String -> String)
atom ae =
  case ae of
    Var i -> if "FUN" `isPrefixOf` (showIdent i) then fun $ read (drop 3 (showIdent i))
               else error "Strange Var exists."
    Lit (LInt i) -> int i
    Lit (LPrim op) -> prim op
    Lit _ -> error "Strange Lit exists."
    Sc a p is -> comb a p is
    _ -> error "Not an Atom."
