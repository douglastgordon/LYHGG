-- 99 Haskell Problems
import Data.List
import Data.Function
import Data.Maybe

-- problem 1
myLast :: [a] -> a
myLast [] = error "Can't find last element of empty list"
myLast [el] = el
myLast (_:rest) = myLast rest

-- problem 2
myButLast :: [a] -> a
myButLast [] = error "Can't find second last element of empty list"
myButLast [x] = error "Can't find second last element of list length 1"
myButLast [secondLast, _] = secondLast
myButLast (_:rest) = myButLast rest

-- problem 3
elementAt :: [a] -> Int -> a
elementAt list k
  | (k > length list) || (k < 1) = error "Index out of bounds"
  | otherwise = list !! (k - 1)

-- problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (first:rest) = 1 + myLength rest

-- problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (first:rest) = myReverse rest ++ [first]

-- problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [el] = True
isPalindrome all@(first:rest) = first == (last all) && isPalindrome (init rest)

-- problem 7 - come back after learning type stuff

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- problem 8
compress :: Eq a => [a] -> [a]
compress = foldr (\el acc -> if (length acc == 0) || (head acc) /= el then el : acc else acc) []

-- problem 9
pack :: Eq a => [a] -> [[a]]
pack = foldl (\acc el -> if (length (last acc)) /= 0 && el /= (last $ last acc)
                         then acc ++ [[el]]
                         else (init acc) ++ [(el : (last acc))]) [[]]

-- problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\all@(x:xs) -> (length all, x)) . pack

-- problem 11
data Encoding v = Single v | Multiple Int v deriving Show
encodeModified :: [(Int, a)] -> [Encoding a]
encodeModified = map (\(n, v) -> if n == 1 then (Single v) else Multiple n v)

-- problem 12
decodeModified :: [Encoding a] -> [(Int, a)]
decodeModified = map (decodeSingle)

decodeSingle :: Encoding a -> (Int, a)
decodeSingle (Single v) = (1, v)
decodeSingle (Multiple n v) = (n, v)

-- problem 13
encodingValue :: Encoding a -> a
encodingValue (Single v) = v
encodingValue (Multiple _ v) = v

encodingCount :: Encoding a -> Int
encodingCount (Single _) = 1
encodingCount (Multiple n _) = n

increaseCount :: Encoding a -> Encoding a
increaseCount (Single v) = Multiple 2 v
increaseCount (Multiple n v) = Multiple (n + 1) v

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect = foldl (\acc el ->
                        if length acc == 0 || encodingValue (last acc) /= el
                          then acc ++ [Single el]
                        else (init acc) ++ [(increaseCount (last acc))]
                      ) []


-- problem 14
dupli :: [a] -> [a]
dupli = foldr (\el acc -> el : el : acc) []

-- problem 15
repli :: [a] -> Int -> [a]
repli list k = foldr (\el acc -> replicate k el ++ acc) [] list

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs k = [el | (el, idx) <- (zip xs [1..]), idx `mod` k /= 0]

-- problem 17 (no predifined predicates)
split :: [a] -> Int -> [[a]]
split list k = [[el | (el, idx) <- tuples, idx <= k], [el | (el, idx) <- tuples, idx > k]]
             where tuples = (zip list [1..])

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice list i k
  | i >= k || i > (length list) || k < 0 = []
slice list i k = [el | (el, idx) <- zip list [1..], idx >= i && idx <= k]

-- problem 19
rotate :: [a] -> Int -> [a]
rotate all@(x:xs) n
  | n == 0 || n `mod` (length all) == 0 = all
  | n > 0 = rotate (xs ++ [x]) (n - 1)
  | n < 0 = rotate ((last all) : (init all)) (n + 1)

-- problem 20
removeAt :: [a] -> Int -> [a]
removeAt list k = [x | (x, idx) <- zip list [1..], idx /= k]

-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt el list k = [x | (x, idx) <- zip list [1..], idx < k] ++ [el] ++ [x | (x, idx) <- zip list [1..], idx >= k]

-- problem 22
range :: Int -> Int -> [Int]
range a b = [a..b]

-- problem 23, 24, 25 - come back after learning randomness

-- problem 26 - 27 - come back after learning probability

-- problem 28a
lsort :: [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

-- problem 28b
-- lfsort :: [[a]] -> [[a]]

-- problem 31
isPrime :: Int -> Bool
isPrime n
  | n < 2 || n /= 2 && n `mod` 2 == 0 = False
  | n == 2 || n == 3 = True
  | otherwise = foldl (\acc divisor -> (n `mod` divisor /= 0) && acc) True [2..(floor . sqrt $ fromIntegral n)]

-- problem 32
myGCD :: Int -> Int -> Int
myGCD a b
  | a == b = a
  | otherwise = myGCD smaller (larger - smaller)
    where smaller = min a b
          larger = max a b

-- problem 33
coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

-- problem 34
totient :: Int -> Int
totient n = length $ filter (coprime n) [1..(n - 1)]

-- problem 35
primeFactors :: Int -> [Int]
primeFactors n = if isPrime n then [n] else spf : primeFactors (n `div` spf)
               where spf = smallestPrimeFactor n

smallestPrimeFactor :: Int -> Int
smallestPrimeFactor n = head $ filter (\x -> n `mod` x == 0 && isPrime x) [2..n]

-- problem 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult = encode . primeFactors

-- problem 37 + 38
phi :: Int -> Int
phi n = foldl (\acc (m, p) -> acc * ((p - 1) * p ^ (m - 1))) 1 (prime_factors_mult n)

-- problem 39
primesR :: Int -> Int -> [Int]
primesR low high = filter isPrime [low..high]

-- problem 40
goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- primes, y <- primes, x + y == n]
             where primes = primesR 0 n

-- problem 41 -- 1489
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach (filter even [a..b])

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- problem 54
-- no problem

-- problem 55
-- cBalTree :: Int -> [(Tree Char)]
-- cBalTree 0 = [Empty]
-- cBalTree 1 = [(Node 'x' Empty Empty)]
-- cBalTree 2 = [Node 'x' (Node 'x' Empty Empty) (Empty), Node 'x' (Empty) (Node 'x' Empty Empty)]
-- cBalTree n = map (\x (a, b) -> (Node 'x' a b)) (if n `mod` 2 == 1
--                                                then [(cBalTree halfN, cBalTree halfN)]
--                                              else [(cBalTree littleHalf, cBalTree bigHalf), (cBalTree bigHalf, cBalTree littleHalf)])
--                                              where halfN = ((n - 1) `div` 2)
--                                                    bigHalf = (ceiling ((n - 1) / 2))
--                                                    littleHalf = (floor ((n - 1) / 2))

-- problem 56
symmetric :: (Eq a) => (Tree a) -> Bool
symmetric (Empty) = True
symmetric (Node _ a b) = mirror a b

mirror :: (Eq a) => (Tree a) -> (Tree a) -> Bool
mirror (Empty) (Empty) = True
mirror (Empty) (Node _ _ _) = False
mirror (Node _ _ _) (Empty) = False
mirror (Node val1 left1 right1) (Node val2 left2 right2) = val1 == val2 && mirror left1 right2 && mirror left2 right1

-- symmetric (Node 'x' (Node 'x' Empty Empty) Empty)
-- False
-- symmetric (Node 'x' (Node 'x' Empty Empty) (Node 'x' Empty Empty))
-- True
