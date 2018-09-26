-- head takes a list and returns its head.
head' :: [a] -> a
head' [] = error "Can't find head of empty list"
head' (first:rest) = first

-- tail takes a list and returns its tail.
tail' :: [a] -> [a]
tail' [] = error "Can't find tail of empty list"
tail' (_:rest) = rest

-- last takes a list and returns its last element.
last' :: [a] -> a
last' [] = error "Can't find last of empty list"
last' [el] = el
last' (_:rest) = last' rest

-- init takes a list and returns everything except its last element.
init' :: [a] -> [a]
init' [] = error "Can't find init of empty list"
init' [_] = []
init' (first:rest) = first:(init' rest)

-- length takes a list and returns its length
length' :: [a] -> Int
length' [] = 0
length' (first:rest) = 1 + length' rest

-- null checks if a list is empty. If it is, it returns True, otherwise it returns False.
null' :: Eq a => [a] -> Bool
null' x = if x == [] then True else False

-- reverse reverses a list. - NON LINEAR IMPLEMENTATION - CAN BE IMPROVED
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (first:rest) = reverse' rest ++ [first]

-- take takes number and a list. It returns that many elements from the beginning of the list.
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (first:rest) = first : take' (n - 1) rest

-- drop takes a number and a list. It returns the elements after the first n elements
drop' :: Int -> [a] -> [a]
drop' 0 list = list
drop' _ [] = []
drop' n (first:rest) = drop' (n - 1) rest

-- maximum takes a list of stuff that can be put in some kind of order and returns the biggest element.
maximum' :: Ord a => [a] -> a
maximum' [] = error "Can't find maximum of empty array"
maximum' [a] = a
maximum' [a, b] = if a > b then a else b
maximum' (first:rest) = maximum' [first, (maximum' rest)]

-- minimum returns the smallest.
minimum' :: Ord a => [a] -> a
minimum' [] = error "Can't find minimum of empty array"
minimum' [a] = a
minimum' [a, b] = if a < b then a else b
minimum' (first:rest) = minimum' [first, (minimum' rest)]

-- sum takes a list of numbers and returns their sum.
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (first:rest) = first + sum' rest

-- product takes a list of numbers and returns their product.
product' :: Num a => [a] -> a
product' [] = error "Can't find product of empty array"
product' [num] = num
product' (first:rest) = first * product' rest

-- elem takes a thing and a list of things and tells us if that thing is an element of the list.
elem' :: Eq a => a -> [a] -> Bool
elem' a [el] = a == el
elem' a (first:rest) = a == first || elem' a rest

-- cycle takes a list and cycles it into an infinite list.
cycle' :: [a] -> [a]
cycle' list = list ++ (cycle' list)

-- repeat takes an element and produces an infinite list of just that element.
repeat' :: a -> [a]
repeat' a = a : (repeat' a)

-- replicate takes a number n and a value and produces a list consisting only of that value, length n
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n el = el : (replicate' (n - 1) el)

-- fst takes a pair and returns its first component.
fst' :: (a, b) -> a
fst' (a, _) = a

-- snd takes a pair and returns its second component.
snd' :: (a, b) -> b
snd' (_, b) = b

-- zip takes two lists and then zips them together into one list by joining the matching elements into pairs.
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:rest1) (b:rest2) = (a, b) : (zip' rest1 rest2)

-- which right triangle that has integers for all sides and all sides
-- equal to or smaller than 10 has a perimeter of 24?
findTriangle :: (Int, Int, Int)
findTriangle = [(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a <= b, b <= c, a + b + c == 24, a^2 + b^2 == c^2] !! 0

-- factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (first:rest) = (quicksort [n | n <- rest, n <= first]) ++ [first] ++ (quicksort [n | n <- rest, n > first])

-- zipWith takes a function and two lists as parameters and then joins the two lists by applying the function between corresponding elements.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' fn (a:rest1) (b:rest2) = (fn a b) : (zipWith' fn rest1 rest2)

-- flip takes a function and returns a function that is like our original function, only the first two arguments are flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' fn a b = fn b a

-- map takes a function and a list and applies that function to every element in the list, producing a new list.
map' :: (a -> b) -> [a] -> [b]
map' fn list = [fn a | a <- list]

-- filter is a function that takes a predicate and a list and then returns the list of elements that satisfy the predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' fn list = [a | a <- list, fn a]

-- find the largest number under 100,000 that's divisible by 3829
-- findNum :: Int
findNum = head (filter' divisible [100000,99999..0])
          where divisible x = x `mod` 3829 == 0

-- takeWhile takes a predicate and a list and then goes from the beginning of the list and returns its elements while the predicate holds true.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' fn (x:xs)
  | fn x  = x : (takeWhile' fn xs)
  | otherwise = []

-- find the sum of all odd squares that are smaller than 10,000
sumOddSquares :: Int
sumOddSquares = sum (takeWhile (<10000) (map (^2) [1,3..]))

-- collatz chain: take a num, if that number is even, we divide it by two. If it's odd, we multiply it by 3 and then add 1 to that.
-- for all starting numbers between 1 and 100, how many chains have a collatz chain length greater than 15?

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n = [n] ++ collatz (if odd n then ((n * 3) + 1) else (n `div` 2))

bigCollatzChains :: Int
bigCollatzChains = length (filter (>15) (map (length . collatz) [1..100]))

-- foldl reduces list to value from left
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' fn acc (x:xs) = foldl' fn (fn acc x) xs

-- foldl1 reduces list to value from left with accumulator inititialized to first val of arr
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = error "Can't reduce empty list"
foldl1' _ [a] = a
foldl1' fn (first:second:xs) = foldl1' fn ((fn first second) : xs)

-- foldr reduces list to value from right
foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' _ acc [] = acc
foldr' fn acc xs = foldr' fn (fn (last xs) acc) (init xs)

-- foldr1 reduces list to value from left with accumulator inititialized to first val of arr
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' _ [] = error "Can't reduce empty list"
foldr1' _ [a] = a
foldr1' fn xs = foldr1' fn (init (init xs) ++ [fn ultimate penultimate])
              where ultimate = last xs
                    penultimate = last (init xs)


maximum'' :: Ord a => [a] -> a
maximum'' = foldl1 (\acc el -> if acc > el then acc else el)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product'' :: Num a => [a] -> a
product'' = foldr1 (*)

-- scanl - like foldl but reports all the intermediate accumulator states in the form of a list.
scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ _ [] = []
scanl' fn acc (x:xs) = (scanl' fn newAcc xs) ++ [newAcc]
                     where newAcc = fn acc x

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
-- sumElements :: Int
sumElements = length $ takeWhile (<1000) (scanl1 (+) (map (**0.5)[1..])) + 1
