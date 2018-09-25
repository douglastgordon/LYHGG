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
