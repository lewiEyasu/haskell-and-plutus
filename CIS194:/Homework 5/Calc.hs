import ExprT
import Parser
import System.Posix.Internals (lstat)
{-Exercise 1
Write Version1of the calculator:an evaluator forExprT, with thesignature-}

eval :: ExprT -> Integer
eval (Add c@(_) (Lit x)) = x + eval c
eval (Mul c@(_) (Lit x)) = x * eval c
eval (Lit x) = x

{- Exercise 2 -}
evalStr :: String -> Maybe Integer
evalStr x = case xs of (Just d) -> Just (eval d)
                       _ -> Nothing  
         where xs = parseExp Lit Add Mul x
{- Exercise 3 -}
class Expr a where
    lit :: Integer -> a 
    add :: a  -> a  -> a
    mul :: a  -> a  -> a

instance Expr ExprT where
    lit = Lit
    add x1  x2 = Add x1  x2
    mul x1 x2 = Mul  x1  x2
   
{- Exercise 4 -}

instance Expr Integer where
    lit  = id 
    add x1  x2 = x1 + x2
    mul x1 x2 = x1 * x2  

instance Expr Bool where
    lit x = x > 0  
    add x1  x2 = x1 || x2
    mul x1 x2 = x1 && x2

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where 
    lit x = MinMax x  
    add (MinMax x1)  (MinMax x2) = MinMax (max x1 x2 )
    mul (MinMax x1)  (MinMax x2) = MinMax (min x1 x2 )

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 x1) (Mod7 x2)  =Mod7 ((x1 `mod` 7) + (x2 `mod` 7))
    mul (Mod7 x1) (Mod7 x2)  =Mod7 ((x1 `mod` 7) * (x2 `mod` 7)) 

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7