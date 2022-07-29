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
    lit x = Lit x 
    add x1  x2 = Add x1  x2
    mul x1 x2 = Mul  x1  x2
   
