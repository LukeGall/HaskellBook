module Chapter5 where
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

