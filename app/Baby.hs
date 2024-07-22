module Baby where
import Text.Read

doubleMe x = x + x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2
doubleSmallNumber' x = doubleSmallNumber x + 1
conanO'Brien = "It's a-me, Conan O'Brien!"
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
factorial :: Integer -> Integer  
factorial n = product [1..n]
circumference :: Float -> Float  
circumference r = 2 * pi * r
circumference' :: Double -> Double
circumference' r = 2 * pi * r
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"
factorial' :: (Integral a) => a -> a  
factorial' 0 = 1  
factorial' n = n * factorial' (n - 1)
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
first :: (a, b, c) -> a  
first (x, _, _) = x  
second :: (a, b, c) -> b  
second (_, y, _) = y  
third :: (a, b, c) -> c  
third (_, _, z) = z
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell [x] = "The list has one element: " ++ show x  
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
length'' :: (Num b) => [a] -> b  
length'' [] = 0  
length'' (_:xs) = 1 + length' xs
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] 
densityTell :: (RealFloat a) => a -> a -> String  
densityTell mass volume  
    | density < air = "Wow! You're going for a ride in the sky!"  
    | density <= water = "Have fun swimming, but watch out for sharks!"  
    | otherwise   = "If it's sink or swim, you're going to sink."
    where density = mass / volume
          air = 1.2  
          water = 1000.0 
max' :: (Ord a) => a -> a -> a  
max' a b | a > b = a | otherwise = b
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT
densityTell' :: String -> String  
densityTell' input =
  case readMaybe input :: Read a => Maybe a of
    Just density | density < 1.2 -> "Wow! You're going for a ride in the sky!"
                 | density <= 1000.0 -> "Have fun swimming, but watch out for sharks!"
    Nothing -> "You know I need a density, right?"
    otherwise -> "If it's sink or swim, you're going to sink."
    where (air, water) = (1.2, 1000.0)

initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname
calcDensities :: (RealFloat a) => [(a, a)] -> [a]  
calcDensities xs = [density m v | (m, v) <- xs]  
    where density mass volume = mass / volume

cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

calcDensities' :: (RealFloat a) => [(a, a)] -> [a]  
calcDensities' xs = [density | (m, v) <- xs, let density = m / v]

calcDens :: (RealFloat a) => [(a, a)] -> [a]  
calcDens xs = [density | (m, v) <- xs, let density = m / v, density < 1.2]

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."  
                                               xs -> "a longer list."
                            
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)  
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]  
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100