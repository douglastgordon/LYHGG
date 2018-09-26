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
