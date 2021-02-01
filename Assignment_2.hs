replaceElement :: (Int,a) -> [a] -> [a]
replaceElement (x,y) z = if (x < 0 || x > ((length z) - 1))
then z
else take x z ++ (y : (drop (x + 1) z))

replaceElements :: [(Int,a)] -> [a] -> [a] 
replaceElements [] xs = xs
replaceElements (y:ys) xs = replaceElements ys ( replaceElement y xs )

addItem :: (String,a) -> [(String,a)] -> [(String,a)]
addItem x xs = x : xs;

subsetByKey :: String -> [(String,a)] -> [a]
subsetByKey _ [] = []
subsetByKey y (x:xs) = (subsetByKey' y x) ++ (subsetByKey y xs)

--auxiliary function 
subsetByKey' :: String -> (String,a) -> [a]
subsetByKey' y (x,xs) = if(y == x) 
then [xs]
else []

subsetByKeys :: [String] -> [(String,a)] -> [a]
subsetByKeys [] y = []
subsetByKeys _ [] = []
subsetByKeys (x:xs) y = (subsetByKey x y) ++ (subsetByKeys xs y)

getKeys :: [(String,a)] -> [String]
getKeys [] = []
getKeys ((x,w):xs) = if (exists x xs) 
then getKeys xs
else x : getKeys xs

--auxiliary function 
exists :: String -> [(String,a)] -> Bool
exists _ [] = False
exists y ((x,w):xs) = if(y == x)
then True
else exists y xs

groupByKeys :: [(String,a)] -> [(String,[a])]
groupByKeys [] = []
groupByKeys xs = groupByKeys' (getKeys xs) xs

--auxiliary function
groupByKeys' :: [String] -> [(String,a)] -> [(String,[a])]
groupByKeys' [] _ = []
groupByKeys' (x:xs) ys = (x,(subsetByKey x ys)) : (groupByKeys' xs ys)

createMatrix :: Int -> Int -> [a] -> [[a]]
createMatrix 0 _ _ = []
createMatrix _ 0 _ = []
createMatrix _ _ [] = []
createMatrix m n xs = (take n xs) : (createMatrix m n (drop n xs))

getElementInCell :: Int -> Int -> [[a]] -> a
getElementInCell m n xs =  (xs !! m) !! n

appendH :: [[a]] -> [[a]] -> [[a]]
appendH [] [] = []
appendH (x:xs) (y:ys) = (x ++ y) : (appendH xs ys)

appendV :: [[a]] -> [[a]] -> [[a]]
appendV [] [] = []
appendV xs ys = xs ++ ys

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices [] [] = []
addMatrices (x:xs) (y:ys) = sumRows x y : addMatrices xs ys

--auxiliary function
sumRows :: [Int] -> [Int] -> [Int]
sumRows [] [] = []
sumRows (x:xs) (y:ys) = (x + y) : sumRows xs ys