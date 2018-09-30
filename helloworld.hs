-- main = do
--   putStrLn "What do you want to say hello to?"
--   object <- getLine
--   putStrLn $ "hello, " ++ object

main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words  
