isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome x = x == reverse x 


isPrefix  :: String -> String -> Bool
isPrefix [] _ = True
isPrefix x [] = False
isPrefix (x:xs) (y:ys) = if x == y
then isPrefix xs ys
else False


squareList :: Int -> [Int]
squareList 0 = [0]
squareList x = product[x,x] : squareList (x-1)


listSquare :: Int -> [Int]
listSquare 0 = [0]
listSquare x = listSquare (x-1) ++ [product[x,x]]


fact :: Integer -> Integer
fact 0 = 1 
fact x = fact' x 1

--auxiliary function
fact' :: Integer -> Integer -> Integer
fact' 0 acc = acc
fact' x acc = fact' (x-1) (acc * x)


toDigits :: Integer -> [Integer]
toDigits x = if(x < 0) 
then []
else toDigits' x []

--auxiliary function
toDigits' :: Integer -> [Integer] -> [Integer]
toDigits' 0 acc = acc
toDigits' x acc = toDigits' (div x 10) ((mod x 10) : acc)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOther' [] x

--auxiliary function
doubleEveryOther' :: [Integer] -> [Integer] -> [Integer]
doubleEveryOther' acc [] = acc
doubleEveryOther' acc x = if (mod (length acc) 2 ==  1) 
then doubleEveryOther' ((last x * 2) : acc) (init x)
else doubleEveryOther' ((last x) : acc) (init x)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits x = sumDigits' 0 x

--auxiliary function
sumDigits' :: Integer -> [Integer] -> Integer
sumDigits' acc [] = acc
sumDigits' acc x = if(last x > 9)
then sumDigits' acc ((toDigits (last x)) ++ (init x))
else sumDigits' (acc + (last x)) (init x)


validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigits x))) 10 == 0