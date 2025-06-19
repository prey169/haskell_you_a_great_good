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
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lowestInt = minBound :: Int
largestChar = maxBound :: Char

largestInTuple = maxBound :: (Bool, Int, Char)
