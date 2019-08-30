module Chapter6 where
import           Data.Char
import           Prelude         hiding (Word, getLine)
import           Test.QuickCheck

getFirstInt :: [Integer] -> Integer
getFirstInt []     = 0
getFirstInt (x:xs) = x+1

sumFirstTwoNums :: [Integer] -> Integer
sumFirstTwoNums []         = 0
sumFirstTwoNums (x:(y:ys)) = x + y
sumFirstTwoNums (x:xs)     = x

myProduct :: [Integer] -> Integer
myProduct []     = 1
myProduct (x:xs) = x * myProduct xs

myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myReverse :: String -> String
myReverse []     = ""
myReverse (x:xs) = myReverse xs ++ [x]

elemNum :: Integer -> [Integer] -> Integer
elemNum x [] = 0
elemNum x (y:ys)
    | x == y = 1 + elemNum x ys
    | otherwise = 0 + elemNum x ys

unique :: [Integer] -> [Integer]
unique xs = [x | x <- xs, (elemNum x xs) == 1]

uniqueRec :: [Integer] -> [Integer]
uniqueRec list = aux list
    where
        aux :: [Integer] -> [Integer]
        aux [] = []
        aux (x:xs)
            | (elemNum x list) == 1 = x: aux xs
            | otherwise = aux xs

myUnzip :: [(a,b)] -> ([a],[b])
myUnzip []         = ([],[])
myUnzip ((a,b):xs) = (a : (fst rest), b : (snd rest))
    where rest = myUnzip xs

isSorted :: [Integer] -> Bool
isSorted []       = True
isSorted [x]      = True
isSorted (x:y:ys) = (x <= y) && (isSorted (y:ys))

iSort :: [(Integer,Integer)] -> [(Integer,Integer)]
iSort []         = []
iSort ((x,y):xs) = ins (x,y) (iSort xs)

ins :: (Integer,Integer) -> [(Integer,Integer)] -> [(Integer,Integer)]
ins (a,b) [] = [(a,b)]
ins (a,b) ((x,y):xs)
    | (a < x) || ( (a==x) && (b<y) )   = (a,b):((x,y):xs)
    | (a > x) || ( (a==x) && (b>y) )   = (x,y):ins (a,b) xs
    | otherwise                        = ((x,y):xs)

myDrop :: Int -> [a] -> [a]
myDrop 0 (x:xs) = (x:xs)
myDrop _ [] = []
myDrop n (x:xs)
    | n>0 = myDrop (n-1) xs
myDrop _ _ = error "Negative"

testMyDrop :: Int -> [Char] -> Bool
testMyDrop n x
    | n > 0 = (myDrop n x) == (drop n x)
    | otherwise = True

zip3Rec :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3Rec (a:as) (b:bs) (c:cs) = (a,b,c) : zip3Rec as bs cs
zip3Rec _ _ _                = []

qSortDec :: [Integer] -> [Integer]
qSortDec [] = []
qSortDec (x:xs)
    = qSortDec [y | y <- xs, y > x] ++ [x] ++ [y | y <- xs, y < x]

sublist :: String -> String -> Bool
sublist (a:as) (b:bs)
    | a == b = sublist as bs
    | otherwise = sublist (a:as) bs
sublist [] _ = True
sublist _ [] = False


startOfList :: String -> String -> Bool
startOfList (a:as) (b:bs)
    | a == b = startOfList as bs
    | otherwise = False
startOfList [] _ = True
startOfList _ [] = False

subsequence :: String -> String -> Bool
subsequence (a:as) (b:bs)
    | a == b = startOfList as bs
    | otherwise = subsequence (a:as) (bs)
subsequence [] _ = True
subsequence _ [] = False

-- Text Processing

whitespace = ['\n','\t',' ']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
    | elem x whitespace = []
    | otherwise = x : getWord xs

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | elem x whitespace = (x:xs)
    | otherwise = dropWord xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
    | elem x whitespace = dropSpace xs
    | otherwise = (x:xs)

type Word = String

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st = (getWord st) : split (dropSpace (dropWord st))

type Line = [Word]

getLine :: Int -> [Word] -> Line
getLine len [] = []
getLine len (w:ws)
    | length w <= len = w :restOfLine
    | otherwise = []
    where
        newlen = len - (length w +1)
        restOfLine = getLine newlen ws
lineLen :: Int
lineLen = 50

dropLine :: Int -> [Word] -> Line
dropLine len [] = []
dropLine len (w:ws)
    | length w <= len = restOfLine
    | otherwise = (w:ws)
    where
        newlen = len - (length w + 1)
        restOfLine = dropLine newlen ws

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = getLine lineLen ws : splitLines (dropLine lineLen ws)

joinLine :: Line -> String
joinLine []     = []
joinLine (w:ws)
    | ws /= [] = w ++ " " ++ joinLine ws
    | otherwise = w

joinLines :: [Line] -> String
joinLines []     = []
joinLines (w:ws) = joinLine w ++ "\n" ++ joinLines ws

ws :: String -> (Int,Int,Int)
ws [] = (0,0,0)
ws str = (chars,words,lines)
    where
        chars = length [x | x <- str, x /= '\n', x/=' ']
        words = length (splitWords str)
        lines = length [x | x <- str, x == '\n']

isPalin :: String -> Bool
isPalin str = noSpace == (myReverse noSpace)
    where noSpace = [toLower c | c <- str, isAlpha c]
