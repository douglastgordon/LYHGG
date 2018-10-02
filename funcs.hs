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
-- sumElements = length $ takeWhile (<1000) (scanl1 (+) (map (**0.5)[1..])) + 1

-- nub takes a list and removes duplicates (from Data.List)
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' [n | n <- xs, n /= x]

-- intersperse takes an element and a list and then puts that element in between each pair of elements in the list. (from Data.List)
intersperse' :: a -> [a] -> [a]
intersperse' a = init . foldr (\el acc -> el : a : acc) []

-- intercalate takes a list of lists and a list. It then inserts that list in between all those lists and then flattens the result. (from Data.List)
intercalate' :: [a] -> [[a]] -> [a]
intercalate' a list = foldl (\acc (el, idx) -> if idx /= length list then acc ++ el ++ a else acc ++ el) [] (zip list [1..])

-- transpose transposes a list of lists. (from Data.List)
transpose' :: [[a]] -> [[a]]
transpose' list = map (\(_, x) -> map (\row -> row !! x) list)  (zip list [0..])


-- concat flattens a list of lists into just a list of elements.(from Data.List)
concat' :: [[a]] -> [a]
concat' = foldl1 (\acc el -> acc ++ el)

-- concatMap is the same as first mapping a function to a list and then concatenating the list with concat (from Data.List)
concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' fn = concat . map fn

-- and takes a list of boolean values and returns True only if all the values in the list are True. (from Data.List)
and' :: [Bool] -> Bool
and' = foldr1 (\el acc -> acc && el)

-- or is like and, only it returns True if any of the boolean values in a list is True. (from Data.List)
or' :: [Bool] -> Bool
or' = foldr1 (\el acc -> acc || el)

-- any and all take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively. (from Data.List)
any' :: (a -> Bool) -> [a] -> Bool
any' fn = or' . map fn

all' :: (a -> Bool) -> [a] -> Bool
all' fn = and' . map fn

-- iterate takes a function and a starting value. It applies the function to the starting value, then it applies that function to the result, then it applies the function to that result again, etc. It returns all the results in the form of an infinite list. (from Data.List)
iterate' :: (a -> a) -> a -> [a]
iterate' fn x = x : iterate fn (fn x)

-- splitAt takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple. (from Data.List)
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n list = ([x | (x, idx) <- zip list [1..], idx <= n], [x | (x, idx) <- zip list [1..], idx > n])

-- dropWhile is similar, only it drops all the elements while the predicate is true.  from Data.List
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' fn all@(x:xs)
    | length all == 0 = []
    | fn x = dropWhile fn xs
    | otherwise = all

-- span
-- break

-- inits and tails are like init and tail, only they recursively apply that to a list until there's nothing left.
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' list = inits' (tail list) ++ [list]

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' all@(x:xs) = [all] ++ tails' xs


-- isInfixOf searches for a sublist within a list and returns True if the sublist we're looking for is somewhere inside the target list.
isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' sublist [] = False
isInfixOf' sublist list@(x:xs) = (sublist == (take (length sublist) list)) || isInfixOf' xs sublist

-- isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively.
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' sublist list = sublist == take (length sublist) list

isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' sublist list = drop (length list - length sublist) list == sublist

-- elem and notElem check if an element is or isn't inside a list.
-- elem' :: Eq a => a -> [a] -> Bool
-- elem' a (x:xs) = a == x || elem' a xs

notElem' :: Eq a =>  a -> [a] -> Bool
notElem' a (x:xs) = a /= x && notElem' a xs

-- partition takes a list and a predicate and returns a pair of lists. The first list in the result contains all the elements that satisfy the predicate, the second contains all the ones that don't.
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' fn = foldl (\(yesses, nos) el -> if (fn el == True)
                                             then (yesses ++ [el], nos)
                                             else (nos, yesses ++ [el])) ([], [])

-- find takes a list and a predicate and returns the first element that satisfies the predicate.
-- find' :: [a] -> (a -> Bool) -> Maybe a
-- find' [] _ = Nothing
-- find' (x:xs) fn = if fn x then Just x else find' xs fn

-- elemIndices is like elemIndex, only it returns a list of indices, in case the element we're looking for crops up in our list several times.
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' el list = [idx | (x, idx) <- zip list [0..], x == el]

-- delete takes an element and a list and deletes the first occurence of that element in the list.
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) = if n == x then xs else x : delete' n xs

data Tree a = EmptyTree |  Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

extractInt :: Maybe (Int) -> Int
extractInt Nothing = 0
extractInt (Just a) = a

-- putStr' :: String -> IO ()
-- putStr'

putChar' :: Char -> IO Char
putChar' x = IO x
