import ExprT
import Parser
{-Exercise1
Write Version1of the calculator:an evaluator forExprT, with thesignature-}

eval :: ExprT -> Integer
eval (Add c@(_) (Lit x)) = x + eval c
eval (Mul c@(_) (Lit x)) = x * eval c
eval (Lit x) = x

evalStr :: String -> Maybe Integer
evalStr x = case xs of (Just d) -> Just (eval d)
                       _ -> Nothing  
         where xs = parseExp Lit Add Mul x
