import ExprT
import Parser
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
    lit :: a -> Integer
    add :: a  -> a  -> Integer
    mul :: a  -> a  -> Integer

instance Expr ExprT where
    lit Lisx =  x 
    add x1  x2 = x1 + x2
    mul x1 x2 = x1 * x2
   
