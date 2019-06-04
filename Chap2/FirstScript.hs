size :: Integer
size = 12+13

square :: Integer -> Integer
square n = n*n

double :: Integer -> Integer
double n = 2*n

example :: Integer
example = double(size - square (2+2))

task4 :: Integer -> Integer
task4 x = square (double x)