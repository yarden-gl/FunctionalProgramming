
-- data types
data Bit = O | I deriving (Eq, Show)

data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree deriving (Eq, Show)

-- synonyms
type Code = [Bit]
type FreqTable = [(Char, Int)]
type Dictionary = [(Char, Code)]
type Encoder = Char -> Code

-- Question 1

insert :: Char -> FreqTable -> FreqTable
insert x [] = [(x,1)]
insert chr ((x,xs):y) = if(chr == x) 
then (x,xs + 1) : y
else (x,xs) : (insert chr y)

-- auxiliary functions
count :: String -> FreqTable
count x = count' x []

count' :: String -> FreqTable -> FreqTable
count' [] lst = lst 
count' (x:xs) lst = count' xs (insert x lst)

-- Question 2

initLeaves :: FreqTable -> [HuffmanTree]
initLeaves [] = []
initLeaves ((chr,frq):lst)  = (Leaf chr frq) : (initLeaves lst)

-- Question 3

buildTree :: [HuffmanTree] -> HuffmanTree
buildTree lst = buildTreeSorted (sort lst)

buildTreeSorted :: [HuffmanTree] -> HuffmanTree
buildTreeSorted [tree1,tree2] = Node ((getInt tree1) + (getInt tree2)) tree1 tree2
buildTreeSorted (x:xs) = buildTreeSorted (insertInPlace (Node (getInt x + (getInt (head xs))) x (head xs)) (drop 1 xs))

-- auxiliary functions

-- insert a huffmantree in its correct place in a sorted tree
insertInPlace :: HuffmanTree -> [HuffmanTree] -> [HuffmanTree]
insertInPlace x [] = [x]
insertInPlace x (y:ys) = if((getInt x) < (getInt y))
then (x:(y:ys))
else (y : (insertInPlace x ys))

-- sort a huffman tree by frequency of node
sort :: [HuffmanTree] -> [HuffmanTree]
sort [] = []
sort (x:xs) = (findMin x xs) : (sort (removeMin x xs))

findMin :: HuffmanTree -> [HuffmanTree] -> HuffmanTree
findMin x [] = x
findMin x (y:ys) = if ((getInt x) < (getInt y))
then findMin x ys
else findMin y ys

removeMin :: HuffmanTree -> [HuffmanTree] -> [HuffmanTree]
removeMin x [] = []
removeMin x (y:ys) = if ((getInt x) < (getInt y))
then (y : (removeMin x ys))
else (x : (removeMin y ys))

getInt :: HuffmanTree -> Int
getInt (Leaf chr int) = int
getInt (Node int x y) = int

-- Question 4

type Path = [Bit]

createDictionary :: HuffmanTree -> Dictionary
createDictionary (Leaf chr int) = [(chr,[])]
createDictionary (Node int l r) = addDirection O (createDictionary l) ++ addDirection I (createDictionary r)

-- auxiliary function
addDirection :: Bit -> [(Char,Path)] -> [(Char,Path)]
addDirection d [] = []
addDirection d ((x,p):ps) = (x, d:p) : addDirection d ps

-- Question 5

createEncoder :: Dictionary -> Encoder
createEncoder [] _ = []
createEncoder ((x,xs):y) z = if(x == z)
then xs
else createEncoder y z

-- Question 6

encode :: String -> (HuffmanTree, Code)
encode word = let tree = buildTree (initLeaves (count word)) in
 (tree,(getCode word (createDictionary tree) []))

-- auxiliary function
getCode :: String -> Dictionary -> Code -> Code
getCode [] _ x = x
getCode (x:xs) dctnry z = (createEncoder dctnry x) ++ (getCode xs dctnry z) 

-- Question 7

decode :: HuffmanTree -> Code -> String
decode tree [] = []
decode tree code = let dctnry = (createDictionary tree) in (getFirst dctnry code : decode tree (removeFirst dctnry code)) 

-- auxiliary functions
getFirst :: Dictionary -> Code -> Char
getFirst [(x,xs)] _ = x
getFirst ((x,y):z) w = if ((take (length y) w) == y)
then x
else getFirst z w

removeFirst :: Dictionary -> Code -> Code
removeFirst [(x,xs)] w = drop (length xs) w
removeFirst ((x,y):z) w = if ((take (length y) w) == y)
then (drop (length y) w) 
else (removeFirst z w)