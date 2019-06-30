module Chapter5 where
import           Data.Char
import           Pictures
import           Test.QuickCheck

maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y
    | x > y = (x,1)
    | x == y = (x,2)
    | otherwise = (y,1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs x y z
    | x == y && y == z = (z,3)
    | z > n = (z,1)
    | z == n = (z,2)
    | otherwise = maxOccurs x y
    where n = fst (maxOccurs x y)

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y) z

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z = min (min x y) z

between :: Integer -> Integer -> Integer -> Bool
between x y z = min x z <= y && y <= max x z

middle :: Integer -> Integer -> Integer -> Integer
middle x y z
    | between x y z = y
    | between y x z = x
    | otherwise = z

orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
orderTriple (x,y,z) = (minThree x y z, middle x y z, maxThree x y z)

prop_orderTriple :: (Integer,Integer,Integer) -> Bool
prop_orderTriple (x,y,z) = i <= j && j <= k
    where
        i = fs (orderTriple (x,y,z))
        j = sn (orderTriple (x,y,z))
        k = th (orderTriple (x,y,z))

        fs :: (Integer,Integer,Integer) -> Integer
        sn :: (Integer,Integer,Integer) -> Integer
        th :: (Integer,Integer,Integer) -> Integer
        fs (a,b,c) = a
        sn (a,b,c) = b
        th (a,b,c) = c

data Shape = Circle Float |
            Rectangle Float Float |
            Triangle Float Float Float
            deriving (Eq,Ord,Show)

perimeter :: Shape -> Float
perimeter (Circle r)       = 2*pi*r
perimeter (Rectangle h w)  = 2 * (h +w)
perimeter (Triangle a b c) = a+b+c

data ShopItem = Item String Integer

isRound :: Shape -> Bool
isRound (Circle _)       = True
isRound (Rectangle _ _)  = False
isRound (Triangle _ _ _) = False

area :: Shape -> Float
area (Circle r)       = pi*r*r
area (Rectangle h w)  = h*w
area (Triangle a b c) = 0.5 * (a*b) * sin(c)

regular :: Shape -> Bool
regular (Circle _)       = True
regular (Rectangle h w)  = h == w
regular (Triangle a b c) = a==b && b ==c

showShape :: Shape -> String
showShape (Circle r)       = "Circle with radius -" ++ show r
showShape (Rectangle h w)  = "Rectangle - " ++ show w ++ show h
showShape (Triangle a b c) = "Triangle - " ++ show a ++ show b ++ show c

type Centre = (Float,Float)

data NewShape = NewCircle Float Centre|
            NewRectangle Float Float Centre|
            NewTriangle Float Float Float Centre
            deriving (Eq,Ord,Show)

move :: Float -> Float -> NewShape -> NewShape
move nx ny (NewCircle r (x,y))       = (NewCircle r (nx,ny))
move nx ny (NewRectangle h w (x,y))  = (NewRectangle h w (nx,ny))
move nx ny (NewTriangle a b c (x,y)) = (NewTriangle a b c (nx,ny))

doubleAll :: [Integer] -> [Integer]
doubleAll l = [n*2 | n <- l]

capitalize :: String -> String
capitalize xs = [toUpper(n) | n <- xs]

capitalizeLetters :: String -> String
capitalizeLetters xs = [toUpper(n) | n <- xs, isLetter(n)]

divisors :: Integer -> [Integer]
divisors n
    | n > 0 = [x | x <-[1..n], (n `rem` x) == 0]
    | otherwise = []

isPrime :: Integer -> Bool
isPrime n
    | n > 0 = (divisors n) == [1,n]
    | otherwise = False

matches :: Integer -> [Integer] -> [Integer]
matches n xs = [x |x <-xs, n == x]

elem :: Integer -> [Integer] -> Bool
elem n xs
    | (matches n xs) == [] = False
    | otherwise = True

joinStrings :: [String] -> String
joinStrings ss = [c | s<-ss, c<-s]

onSeparateLines :: [String] -> String
onSeparateLines ss = joinStrings [s ++ "\n" | s <- ss]

duplicate :: String -> Integer -> String
duplicate s i
    | i <= 0 = ""
    | otherwise = [c | x <- [0..i-1], c <-s]

pushRight :: String -> String
pushRight s = [' ' | x <- [0.. (ll - length s)]] ++ s
    where ll = 12

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibPair :: Integer -> (Integer, Integer)
fibPair n = (n,fib n)

fibtable :: Integer -> String
fibtable n = "n         fib n\n" ++ onSeparateLines rows
    where
        rows = [row a b | (a,b) <- pairs]
        row a b = (show a) ++ "         " ++ (show b)
        pairs = [fibPair x | x <- [0..n]]

-- Picture exercises

superimposeChar :: Char -> Char -> Char
superimposeChar c1 c2 = if c1=='.' && c2=='.' then '.' else '#'

superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine ln1 ln2 = [superimposeChar ch1 ch2 | (ch1,ch2) <- zip ln1 ln2]

superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2 = [superimposeLine ln1 ln2 | (ln1,ln2) <- zip pic1 pic2]

myPrintPicture :: Picture -> IO ()
myPrintPicture pic = putStr (onSeparateLines pic)

getIthEle :: Int -> [[a]] -> [a]
getIthEle n lines = [l !! n | l <- lines]

rotate90 :: Picture -> Picture
rotate90 pic = [reverse (getIthEle n pic) | n <- [0 .. (length line) - 1]]
    where line = pic !! 0

rotate90anti :: Picture -> Picture
rotate90anti pic =flipH( flipV (rotate90 pic))

scaleLine :: [Char] -> Int -> [Char]
scaleLine line i = concat [replicate i ch | ch <- line]

scaleLines :: Picture -> Int -> Picture
scaleLines pic i = [ (scaleLine l i) | l <- pic]

myScale :: Picture -> Int -> Picture
myScale pic i = rotate90anti (scaleLines (rotate90 (scaleLines pic i)) i)

type Position = (Int,Int)
type Image = (Picture,Position)

makeImage :: Picture -> Position -> Image
makeImage pic pos = (pic,pos)

changePosition :: Image -> Position -> Image
changePosition (pic,oldPos) newPos = (pic,newPos)

moveImage :: Image -> Int -> Int -> Image
moveImage (pic,(x,y)) dx dy = (pic, (x+dx,y+dy))

printImage :: Image -> IO ()
printImage (pic,pos) = myPrintPicture pic

newFlipH :: Image -> Image
newFlipH (pic,(x,y)) = (flipH pic,(-x,y))

newFlipV :: Image -> Image
newFlipV (pic,(x,y)) = (flipV pic, (x,-y))

-- Supermarket billing exercises

type Name = String
type Price = Int
type BarCode = Int

type Database = [ (BarCode,Name,Price) ]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers", 121),
            (1234,"Dry Sherry, 1lt",100),
            (5643, "Nappies", 1010),
            (3814, "Orange Jelly", 56),
            (1111, "Hula Hoops", 21),
            (1112, "King prawns", 499),
            (9000, "Innis and Gunn - Lager", 199)]

type TillType = [BarCode]
type BillType = [(Name,Price)]

lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence p = (show (p `div` 100)) ++ "." ++ (show (p `mod` 100))

formatLine :: (Name,Price) -> String
formatLine (n,p) = n ++ (replicate x '.') ++ (formatPence p) ++ ['\n']
    where x = lineLength - (length n) - (length (formatPence p))

formatLines :: [ (Name,Price) ] -> String
formatLines pairs = concat [formatLine (n,p) | (n,p) <- pairs]

makeTotal :: BillType -> Price
makeTotal pairs = (sum [x | (n,x) <- pairs]) - makeDiscount pairs

formatTotal :: Price -> String
formatTotal t = "\nTotal"++(replicate x '.')++(formatPence t)++"\n"
    where x = lineLength - 5 - (length (formatPence t))

formatBill :: BillType -> String
formatBill bt = (formatLines bt) ++ (formatDiscount (makeDiscount bt)) ++ (formatTotal (makeTotal bt))

look :: Database -> BarCode -> (Name,Price)
look db bc
    | list /= [] = head list
    | otherwise = ("Unknown Item",0)
    where list = [(n,p) | (b,n,p) <- db, bc == b]

myLookup :: BarCode -> (Name,Price)
myLookup bc = look codeIndex bc

makeBill :: TillType -> BillType
makeBill tt = [(n,p) | (n,p) <- list, n /= "Unknown Item"]
    where list = [myLookup bc | bc <- tt]

makeDiscount :: BillType -> Price
makeDiscount bt
    | length list /= 0 = ((length list) `div` 2)*100
    | otherwise = 0
    where list = [name | (name,price) <- bt, name == "Dry Sherry, 1lt"]

formatDiscount :: Price -> String
formatDiscount p
    | p /= 0 = "\nDiscount" ++ (replicate x '.')++(formatPence p)++"\n"
    | otherwise = ""
    where x = lineLength - 8 - (length (formatPence p))
