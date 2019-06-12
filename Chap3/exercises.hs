module Chapter3 where

ex1 :: Bool -> Bool -> Bool
ex1 x y = x && not y || not x && y

ex3 :: Bool -> Bool -> Bool
ex3 True  True  = False
ex3 False False = False
ex3 True  False = True
ex3 False True  = True

myOr :: Bool -> Bool -> Bool
myOr True  True  = True
myOr True  False = True
myOr False True  = True
myOr False False = False

ex5 :: Bool -> Bool -> Bool
ex5 x y = not (x && y)

prop_myOr :: Bool -> Bool -> Bool
prop_myOr x y = (x || y) == myOr x y

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent m n p = m /= n && n /= p && m /= p

fourEqual :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual m n p q = threeEqual m n p && n == q
    where threeEqual m n p = (m == n) && (n == p)

minn :: Integer -> Integer -> Integer
minn x y
    | x < y = x
    | otherwise = y

toLower :: Char -> Char
toLower ch = toEnum (fromEnum ch - (fromEnum 'A' - fromEnum 'a'))

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

charToNum :: Char -> Int
charToNum ch
    | isDigit ch = fromEnum ch - 48
    | otherwise = 0

averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = fromInteger (a+b+c) / 3

numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
    | b^2 > 4.0*a*c = 2
    | b^2 == 4.0*a*c = 1
    | otherwise = 0

numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c
    | a /= 0.0 = numberNDroots a b c
    | b /= 0.0 = 1
    | b == 0.0 && c /= 0.0 = 0
    | otherwise = 100

smallerRoot, largerRoot :: Float -> Float -> Float -> Float

smallerRoot a b c
    | numberRoots a b c == 0 = 0.0
    | otherwise = (-b - sqrt(b^2 - 4*a*c))/2*a

largerRoot a b c
    | numberRoots a b c == 0 = 0.0
    | otherwise = (-b + sqrt(b^2 - 4*a*c))/2*a