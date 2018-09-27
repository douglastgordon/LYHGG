-- 99 Haskell Problems

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

-- problems 11 - 13 - come back after learning type stuff

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

-- problem 26
-- combinations :: k -> [a] -> [[a]]
-- combinations 0 _ = [[]]
-- combinations k list = map (\el -> el : co)
