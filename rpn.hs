solveRPN :: String -> Float
solveRPN str = head $ foldl fn [] (words str)
             where fn (a:b:cs) "+" = (a + b) : cs
                   fn (a:b:cs) "-" = (b - a) : cs
                   fn (a:b:cs) "*" = (a * b) : cs
                   fn (a:b:cs) "/" = (b / a) : cs
                   fn stack el = read el : stack
