
module MicroHs.FunAsm where


import Data.List
import Control.Monad
import Control.Applicative
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Ident
import Data.Char
import Data.Bits 
import Data.Word
import Numeric

data AsmInsn = Root 
             | Ret 
             | CRet
             | AsmInt String Int
             | UInt Int
             | CLi String Int 
             | Fix 
             | Cata 
             | Ana
             | Hylo 
             | Para 
             | Apo 
             | Histo
             | Combi1 Int Int [Int]
             | Combi2 Int Int [Int]
             | Link Int
             | SLink String
             | ELink String
             deriving(Show,Eq)


subsX :: Exp -> Exp
subsX (App e1 e2) = App (subsX e1) (subsX e2)
subsX (Sc n X ls) = Sc (n+1) (At X X) (ls++[n])
subsX e  = e

convPat :: Pat -> [Pat] -> Int -> Int
convPat pat [] _ = error("Pattern not found: "++ (show pat))
convPat pat (x:xs) n | x == pat = n
                     | otherwise = convPat pat xs (n+1) 

sizes :: [[Pat]]
sizes = [X] : (map go . drop 1 . inits) sizes  where
    go smaller = do
      (ls, rs) <- zip smaller (reverse smaller)
      liftM2 At ls rs


replaceChars c = case c of
                 '.' -> "_"
                 '[' -> "Nil"
                 ']' -> ""
                 ':' -> "Cons"
                 x   -> x:[]


replaceDot :: String -> String
replaceDot ls = concat (map replaceChars ls) 

inorder :: [Pat] -> Exp -> [AsmInsn]
inorder pat (App e1 (Sc a p ls)) = [Link (length ee1 +1 )] ++ (ee1) ++ [Combi1 (convPat p pat 0) a ls ,Combi2 (convPat p pat 0) a ls ] where ee1 = inorder pat e1
inorder pat (App e1 (Var id)) = [SLink (replaceDot(showIdent id))] ++ inorder pat e1
inorder pat (App e1 ( Lit (LInt i)))  = [Link (length ee1 +1 )] ++ ee1 ++ [AsmInt "" i,Ret] where ee1 = inorder pat e1
inorder pat (App e1 (Lit (LPrim "ret")) )  = [CRet] ++ (ee1) where ee1 = inorder pat e1 --[Fix] ++ inorder pat e1
inorder pat (App e1 (Lit (LPrim "Y")) )  = [Link (length ee1 +1 )] ++ (ee1) ++ [Fix] where ee1 = inorder pat e1 --[Fix] ++ inorder pat e1
inorder pat (App e1 (Lit (LPrim id))) = [SLink (asmBuiltIn id)] ++ inorder pat e1  
inorder pat (App e1 e2 ) = [Link (length ee1 +1 )] ++ (ee1) ++ (inorder pat e2) where ee1 = inorder pat e1
inorder _ (Var id) = [ ELink (replaceDot(showIdent id)) ]
inorder pat (Lit (LPrim id)) = [SLink (asmBuiltIn id)]
inorder pat (Sc a p ls) = [Combi1 (convPat p pat 0) a ls ,Combi2 (convPat p pat 0) a ls]
inorder _ e = error(show e)
--inorder (Var id) = [ SLink (showIdent id) ]

asmBuiltIn :: String -> String
asmBuiltIn "+" = "call_add"
asmBuiltIn "-" = "call_sub"
asmBuiltIn "*" = "call_mul"
asmBuiltIn "/" = "call_div"
asmBuiltIn "<" = "call_lt"
asmBuiltIn ">" = "call_gt"
asmBuiltIn "==" = "call_eq"
asmBuiltIn "<=" = "call_leq"
asmBuiltIn ">=" = "call_geq"
asmBuiltIn "gpio0_out" = "fn_out"
asmBuiltIn "ret" = "ret"
asmBuiltIn e = error(e)


makesHex :: Integer -> String
makesHex x = take (8-l) ['0','0'..] ++ hex 
             where hex = showHex x ""
                   l = length hex 
                   

--convThunk (Combi t a l _)  =  "        .word \t0x" ++ (makesHex((list2hex l) .|. (arity2hex a) .|. (hex2funct6 t) .|. eval .|. (opcodeThunk))) ++ "\n"

--convSpine (Combi t a l _)  =  "        .word \t0x" ++ (makesHex((list2hex l) .|. (arity2hex a) .|. (hex2funct6 t) .|. eval .|. (opcodeSpine))) ++ "\n"



pprintProg :: [(Ident, [AsmInsn])] -> [String]
pprintProg [] = []
pprintProg ((id, ls):xs) = ((replaceDot(showIdent id))++":\n" ++ (pprintFun ls )) : (pprintProg xs) 

pprintFun :: [AsmInsn]  -> String
pprintFun xs = concat (map pprintAsm xs)

pprintAsm :: AsmInsn -> String
pprintAsm (Combi1 pat a ids) = "\tcomb1 T" ++ (show pat) ++ "," ++ (show a) ++","++ (show ids) ++"\n"
pprintAsm (Combi2 pat a ids) = "\tcomb2 T" ++ (show pat) ++ "," ++ (show a) ++","++ (show ids) ++"\n" 
pprintAsm (SLink id) = "\tlink " ++ id ++"\n" 
pprintAsm (ELink id) = "\telink " ++ id ++"\n"
pprintAsm (AsmInt st n) = "\t.int " ++ (show n)++"\n"
pprintAsm (Ret) = "\tfret\n"--"\t.word 0x0000b567\n"
pprintAsm (CRet) = "\tret\n"--"\t.word 0x0000b567\n"
pprintAsm (Fix) = "\tfn.y\n"
pprintAsm (Link n) =  "\tlink .+" ++ (show (n*4)) ++"\n"
pprintAsm e = "\t" ++ (show e) ++"\n"  
 


hprintProg :: [(Ident, [AsmInsn])] -> [String]
hprintProg [] = []
hprintProg ((id, ls):xs) = ((replaceDot(showIdent id))++":\n" ++ (hprintFun ls )) : (hprintProg xs) 

hprintFun :: [AsmInsn]  -> String
hprintFun xs = concat (map hprintAsm xs)

hprintAsm :: AsmInsn -> String
hprintAsm (Combi1 pat a ids) = "\t.word \t0x" ++ (makesHex((list2hex ids) .|. (arity2hex a) .|. (hex2funct6 pat) .|. eval .|. (opcodeThunk))) ++ "\n"
hprintAsm (Combi2 pat a ids) = "\t.word \t0x" ++ (makesHex((list2hex ids) .|. (arity2hex a) .|. (hex2funct6 pat) .|. eval .|. (opcodeSpine))) ++ "\n"
hprintAsm (SLink id) = "\tlink " ++ id ++"\n" 
hprintAsm (ELink id) = "\telink " ++ id ++"\n"
hprintAsm (AsmInt st n) = "\t.word \t0x" ++ (makesHex(((shiftL (toInteger n) 4) .|. 0x00000006))) ++ "\n"
hprintAsm (Ret) = "\t.word 0x0000b567\n"
hprintAsm (CRet) = "\t.word 0x0000b567\n"
hprintAsm (Fix) = "\tfn.y\n"
hprintAsm (Link n) =  "\tlink .+" ++ (show (n*4)) ++"\n"
hprintAsm e = "\t" ++ (show e) ++"\n"  

list2hex :: [Int] -> Integer
list2hex ls = foldr (.|.) 0 (mapshift ls 14)

mapshift :: [Int] -> Int -> [Integer]
mapshift [] _ = []
mapshift (x:xs) n = (shiftL (toInteger x) n) : (mapshift xs (n+3)) 

arity2hex :: Int -> Integer 
arity2hex n = (shiftL (toInteger n) 11)

hex2funct6 :: Int -> Integer
hex2funct6 0 = 0x00000000  
hex2funct6 1 = 0x00000020
hex2funct6 2 = 0x00000040 
hex2funct6 3 = 0x00000060  
hex2funct6 4 = 0x00000080  
hex2funct6 5 = 0x000000a0  
hex2funct6 6 = 0x000000c0  
hex2funct6 7 = 0x000000e0  
hex2funct6 8 = 0x00000100  
hex2funct6 9 = 0x00000120  
hex2funct6 10 = 0x00000140 
hex2funct6 11 = 0x00000160 
hex2funct6 12 = 0x00000180 
hex2funct6 13 = 0x000001a0 
hex2funct6 14 = 0x000001c0 
hex2funct6 15 = 0x000001e0 
hex2funct6 16 = 0x00000200 
hex2funct6 17 = 0x00000220 
hex2funct6 18 = 0x00000240 
hex2funct6 19 = 0x00000260 
hex2funct6 20 = 0x00000280 
hex2funct6 21 = 0x000002a0 
hex2funct6 22 = 0x000002c0  
hex2funct6 23 = 0x000002e0 
hex2funct6 24 = 0x00000300  
hex2funct6 25 = 0x00000320  
hex2funct6 26 = 0x00000340  
hex2funct6 27 = 0x00000360  
hex2funct6 28 = 0x00000380  
hex2funct6 29 = 0x000003a0  
hex2funct6 30 = 0x000003c0  
hex2funct6 31 = 0x000003e0  
hex2funct6 32 = 0x00000400  
hex2funct6 33 = 0x00000420  
hex2funct6 34 = 0x00000440  
hex2funct6 35 = 0x00000460  
hex2funct6 36 = 0x00000480  
hex2funct6 37 = 0x000004a0  
hex2funct6 38 = 0x000004c0 
hex2funct6 39 = 0x000004e0
hex2funct6 40 = 0x00000500  
hex2funct6 41 = 0x00000520 
hex2funct6 42 = 0x00000540 
hex2funct6 43 = 0x00000560 
hex2funct6 44 = 0x00000580 
hex2funct6 45 = 0x000005a0 
hex2funct6 46 = 0x000005c0 
hex2funct6 47 = 0x000005e0 
hex2funct6 48 = 0x00000600 
hex2funct6 49 = 0x00000620 
hex2funct6 50 = 0x00000640 
hex2funct6 51 = 0x00000660 
hex2funct6 52 = 0x00000680 
hex2funct6 53 = 0x000006a0 
hex2funct6 54 = 0x000006c0 
hex2funct6 55 = 0x000006e0 
hex2funct6 56 = 0x00000700 
hex2funct6 57 = 0x00000720 
hex2funct6 58 = 0x00000740 
hex2funct6 59 = 0x00000760 
hex2funct6 60 = 0x00000780 
hex2funct6 61 = 0x000007a0 
hex2funct6 62 = 0x000007c0 
hex2funct6 63 = 0x000007e0    
hex2funct6 _    = error("Combinator type not found")

opcodeThunk = 0x0000000a
opcodeSpine = 0x0000000e

eval        = 0x00000010
opcodeLink  = 0x00000000
opcodeLit   = 0x00000006

--concat ([SLink ((show n)++"f")]:[toAsm st e1 (concat (toAsm st e2 [] 1))(n+1)])


--convPat :: Pat -> Int
--convPat 




