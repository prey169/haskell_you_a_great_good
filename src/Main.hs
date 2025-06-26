import Text.Read (readMaybe)
import System.IO  

doubleMe x = x + x
doubleUs x y = x * 2 + y * 2
doubleSmallNumber x = if x > 100
                      then x
                      else x * 2
doubleSmallNumber' x = succ (if x > 100
                      then x
                      else x * 2)
conanO'Brien = "It's a me, Conan O' Brian!"
-- The ' is a valid char for functions
-- usually used to denote changes in
-- the original function's function
lostNumbers = [4,8,15,16,23,42]
comprehensionExample = [x * 2| x <- [1..10]]
conditionExample = [x * 2| x <- [1..10], x * 2 >= 12]
filterExample = [x| x <- [50..100], x `mod` 7 == 3]
-- boom and bangs for only odd numbers
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!"| x <- xs, odd x]
length' xs = [1 | _ <- xs] 
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
triangles = [(a, b, c)| c <- [1..10], b <- [1..10], a <- [1..10]]
rightTriangles = [(a, b, c)| c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [(a, b, c)| c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b+ c == 24]

-- Proper way to create functions
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = n * factorial (n - 1)

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lowestInt = minBound :: Int
largestChar = maxBound :: Char

largestInTuple = maxBound :: (Bool, Int, Char)

first :: (a, b, c) -> a
first (x, _, _) = x

head' :: [a] -> a
head' [] = error "Can't do that baka!"
head'(x:_) = x

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- Guards are fancy match statements
densityTell :: (RealFloat a) => a -> String
densityTell density
  | density <= 1.2 = "Wow! You're going for a ride in the sky!"
  | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
  | otherwise = "If it's sink or swim, you're going to sink."

densityTell' :: String -> String
densityTell' input
  | Just density <- readMaybe input :: Maybe Float, density <= 1.2 = "Wow! You're going for a ride in the sky!"
  | Just density <- readMaybe input :: Maybe Float, density  <= 1000.0 = "Have fun swimming, but watch out for sharks"
  | Nothing <- readMaybe input :: Maybe Float = "You know I need a density, right?"
  | otherwise = "If it's sink or swim, you're going to sink."

densityTell'' :: (RealFloat a) => a -> a -> String
densityTell'' mass volume
  | density <= air = "Wow! You're going for a ride in the sky!"
  | density <= water = "Have fun swimming, but watch out for sharks!"
  | otherwise = "If it's sink or swim, you're going to sink."
  where density = mass / volume
        (air, water) = (1.2, 1000.0)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea

calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density | (m, v) <- xs, let density = m / v]

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) =
--  let smallerSorted = quicksort [a | a <- xs, a <= x]
--      biggerSorted = quicksort [a | a <- xs, a > x]
--  in smallerSorted ++ [x] ++ biggerSorted

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<=x) xs)
      biggerSorted = quicksort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- Both of these are the same
-- sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))
-- but with list conprehensions....
-- sum (takeWhile (< 10000) [n^2 | n <- [1..], odd (n^2)])
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int 
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- we can also use lambdas to express the same thing
-- numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
-- 
-- we can take any number of parameters
-- zipWith (\a b -> (a * 30 + 3) /b) [5,4,3,2,1][1,2,3,4,5]
--
-- and pattern match
-- map (\(a, b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

-- these 2 are equivalent
-- addThree x y z = x + y + z
-- and the following (but dont actually use that lol)
-- addThree = \x -> \y -> \z -> x + y + z
--
-- but sometimes it makes sense such as
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

sum''' :: (Num a) => [a] -> a
sum''' xs = foldl (+) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- we could also use the right fold below, but for new lists 
-- it is cheaper to use a right fold as the left fold would 
-- require the ++ operator and its more expensive than the :
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x: acc) [] xs

-- scanl and scanr are the same as folds, but also shows elements 
-- from each step within the fold 
-- scanl (+) 0 [3,5,2,1]
-- would output: [0,3,8,10,11]
-- and scanr (+) 0 [3,5,2,1]
-- would output [11, 8, 5, 3, 0]
sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000)(scanl1 (+) (map sqrt[1..]))) + 1
-- $ lets us rewrite the above as 
-- sqrtSums = length (takeWhile (< 1000) $ scanl1 (+) $ map sqrt[1..] ) + 1
-- $ is also a function so the following works
-- map ($ 3) [(4+), (10*), (^2), sqrt]
--
-- Just putting notes for the (.) function composition operator as well
-- This is similar to doing this after that, for example:
-- x = square . (+x)
-- instead of x y = square (add x y)
--
-- Sum from earlier can omit the xs because of currying and
sum'''' :: (Num a) => [a] -> a
sum'''' = foldl (+) 0

