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
