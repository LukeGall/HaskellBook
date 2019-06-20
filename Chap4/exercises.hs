module Chapter4 where
import           Pictures


maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y) z

maxFour1 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour1 a b c d
    | maxThree a b c > d = maxThree a b c
    | otherwise = d

maxFour2 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour2 a b c d = max (max (max a b) c) d

maxFour3 :: Integer -> Integer -> Integer -> Integer -> Integer
maxFour3 a b c d = max (maxThree a b c) d

prop_maxFour :: Integer -> Integer -> Integer -> Integer -> Bool
prop_maxFour a b c d = (maxFour1 a b c d) == (maxFour2 a b c d) && (maxFour2 a b c d) == (maxFour3 a b c d)

between :: Integer -> Integer -> Integer -> Bool
between a b c = (a < b) && (b < c)

howManyEqual :: Integer -> Integer -> Integer
howManyEqual a b
    | a == b = 2
    | otherwise = 0

howManyEqual3 :: Integer -> Integer -> Integer -> Integer
howManyEqual3 a b c
    | a == b && b == c = 3
    | otherwise = maxThree (howManyEqual a b) (howManyEqual b c) (howManyEqual c a)

howManyEqual4 :: Integer -> Integer -> Integer -> Integer -> Integer
howManyEqual4 a b c d
    | a == b && b == c && c == d = 4
    | otherwise = maxFour1 (howManyEqual3 a b c) (howManyEqual3 a b d) (howManyEqual3 a d c) (howManyEqual3 d b c)

triArea :: Float -> Float -> Float -> Float
triArea a b c
    | possible = sqrt(s*(s-a)*(a-b)*(s-c))
    | otherwise = 0
    where
        s = (a+b+c)/2
        possible = (a>0) && (b>0) && (c>0) && (a<b+c) && (b<a+c) && (c<a+b)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer,Integer)
maxThreeOccurs a b c = (maxN,occurs)
    where
        maxN = maxThree a b c
        onlyOccur :: Integer -> Integer -> Integer -> Bool
        onlyOccur x y z= (x == maxN) && y /=maxN && z /=maxN
        occurs
            | a == b && b == c = 3
            | onlyOccur a b c || onlyOccur b a c || onlyOccur c a b = 1
            | otherwise = 2

data Result = Win | Lost | Draw
    deriving (Show,Eq)


data Move = Rock | Paper | Scissors
    deriving (Show,Eq)

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock  = Scissors
lose Paper = Rock
lost _ = Paper

outcome :: Move -> Move -> Result
outcome x y
    | beat x == y = Lost
    | lose x == y = Win
    | otherwise = Draw

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
    | n < m = 0
    | m == n = n
    | otherwise = m * rangeProduct (m+1) n

difFac :: Integer -> Integer
difFac x = rangeProduct 1 x

myMult :: Integer -> Integer -> Integer
myMult x y
    | y == 0 || x == 0 = 0
    | y == 1 = x
    | otherwise = x + myMult x (y-1)

mySqrt :: Integer -> Integer
mySqrt n = aux n
    where
        aux x
            | x*x > n = aux (x-1)
            | otherwise = x

f :: Integer -> Integer
f 0 = 0
f 1 = 44
f 2 = 17
f 3 = 55
f 4 = 9
f _ = 0

maxFun :: Integer -> Integer
maxFun n = aux n
    where
        aux x
            | x == 0 = n
            | f x > f n = maxFun x
            | otherwise = aux (x-1)


zeroFun :: Integer -> Bool
zeroFun n = aux n
    where
        aux x
            | x == 0 = False
            | f x == 0 = True
            | otherwise = aux (x-1)

blackSquares :: Integer -> Picture
blackSquares n
    | n <= 1 = black
    | otherwise = black `beside` blackSquares (n-1)

whiteSquares :: Integer -> Picture
whiteSquares n
    | n <= 1 = white
    | otherwise = white `beside` whiteSquares (n-1)

blackWhite :: Integer -> Picture
blackWhite n
    | n <= 1 = black
    | otherwise = black `beside` whiteBlack (n-1)

whiteBlack :: Integer -> Picture
whiteBlack n
    | n <= 1 = white
    | otherwise = white `beside` blackWhite (n-1)

blackChess :: Integer -> Integer -> Picture
blackChess n m
    | n <= 1 = blackWhite m
    | otherwise = blackWhite m `above` whiteChess (n-1) m

whiteChess :: Integer -> Integer -> Picture
whiteChess n m
    | n <= 1 = whiteBlack m
    | otherwise = whiteBlack m `above` blackChess (n-1) m

column :: Picture -> Integer -> Picture
column p n
    | n <= 1 = p
    | otherwise = p `above` column p (n-1)

leftDiagonal :: Integer -> Picture
leftDiagonal n = aux n n
    where
        aux :: Integer -> Integer -> Picture
        aux pos len
            | pos <= 1 = black `beside` whiteSquares (len-pos)
            | pos == len = aux (pos-1) len `above` ( whiteSquares (pos - 1) `beside` black)
            | otherwise = aux (pos-1) len `above` ( whiteSquares (pos - 1) `beside` black `beside` whiteSquares (len-pos) )


remainder :: Integer -> Integer -> Integer
divide :: Integer -> Integer -> Integer

remainder m n
    | m < n = m
    | otherwise = remainder (m-n) n

divide m n
    | m < n = 0
    | otherwise = 1 + divide (m-n) n

commonFactors :: Integer -> Integer -> Integer
commonFactors x y = aux m n
    where
        m = max x y
        n = min x y
        aux :: Integer -> Integer -> Integer
        aux i j
            | (remainder i j) == 0 = j
            | otherwise = commonFactors (divide i j) (remainder i j)


