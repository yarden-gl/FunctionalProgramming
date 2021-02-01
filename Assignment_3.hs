
data Expr = Const Value| Add Expr Expr | Mul Expr Expr |Sub Expr Expr| Div Expr Expr | Var Variable 

type Variable = String
type Value = Float

type Dictionary = [(Variable, Value)]
type EvalError = [Variable]
type EvalResult = Either EvalError Value

display :: Expr -> String
display (Const x) = (show x)
display (Add x y) = display' ((display x) ++ "+" ++ (display y))
display (Mul x y) =  display' ((display x) ++ "*" ++ (display y))
display (Sub x y) = display' ((display x) ++ "-" ++ (display y))
display (Div x y)= display' ((display x) ++ "/" ++ (display y))
display (Var x) = x

--auxiliary function
display' :: String -> String
display' x = '(' : x ++ ")"


eval :: Dictionary -> Expr -> EvalResult
eval z (Const x) = Right x
eval z (Var x) = if(varExists z x) 
then Right (findValue z x)
else Left [x]
eval z (Add x y) = newAdd (eval z x) (eval z y)
eval z (Sub x y) = newSub (eval z x) (eval z y)
eval z (Mul x y) = newMul (eval z x) (eval z y)
eval z (Div x y) = newDiv (eval z x) (eval z y)

--auxiliary functions

varExists :: Dictionary -> String -> Bool
varExists [] x = False
varExists ((x,z):xs) y = if(x == y) 
then True
else varExists xs y

printLeft :: EvalResult -> EvalResult -> EvalResult
printLeft(Left x) (Left y) = Left (x ++ y)
printLeft _ (Left y) = Left y
printLeft (Left x) _ = Left x

newAdd :: EvalResult -> EvalResult -> EvalResult
newAdd (Right x) (Right y) = Right (x + y)
newAdd x y = printLeft x y

newSub :: EvalResult -> EvalResult -> EvalResult
newSub (Right x) (Right y) = Right (x - y)
newSub x y = printLeft x y

newMul :: EvalResult -> EvalResult -> EvalResult
newMul (Right x) (Right y) = Right (x * y)
newMul x y = printLeft x y

newDiv :: EvalResult -> EvalResult -> EvalResult
newDiv (Right x) (Right y) = Right (x / y)
newDiv x y = printLeft x y

findValue :: Dictionary -> Variable -> Float
findValue ((x,z):xs) y = if(x == y) 
then z
else findValue xs y


data Tree a b = Leaf b| Node a (Tree a b) (Tree a b) deriving (Show)

reverseTree :: Tree a b -> Tree a b
reverseTree (Leaf x) = Leaf x 
reverseTree (Node x y z) = (Node x) (reverseTree z) (reverseTree y)


isSubtree :: Tree Int Char -> Tree Int Char -> Bool
isSubtree (Leaf x) (Leaf y) = (x == y) 
isSubtree (Leaf x) (Node y z w) = ((isSubtree (Leaf x) z) || (isSubtree (Leaf x) w))
isSubtree (Node x y z) (Leaf w) = False
isSubtree (Node x y z) (Node xs ys zs) = (((x == xs) && (isSubtree y ys) && (isSubtree z zs)) || (isSubtree (Node x y z) ys) || (isSubtree (Node x y z) zs))


data MTree a = MTree a [MTree a] deriving Show

sumMTree :: MTree Int -> Int
sumMTree (MTree x []) = x
sumMTree (MTree x (y:ys)) = x + (sumMTree y) + sumMTree (MTree 0 ys)


grow :: MTree a -> MTree a
grow (MTree x []) = (MTree x [])
grow (MTree x y) = MTree x (replace (MTree x y) y)

-- auxiliary function
replace :: MTree a -> [MTree a] -> [MTree a]
replace x [] = []
replace x ((MTree z zs) : y) = if(length zs == 0) --if node is a leaf
then x : (replace x y) 
else x : (replace x y)