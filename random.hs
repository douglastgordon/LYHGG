import System.Random

threeCoinFlips :: StdGen -> (Bool, Bool, Bool)
threeCoinFlips gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, _) = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

tenRandomNums :: StdGen -> [Int]
tenRandomNums gen = take 10 $ randoms gen

rollDie :: StdGen -> Int
rollDie gen = fst $ randomR (1, 6) gen

rollDie' :: StdGen -> IO ()
rollDie' gen = do
  let (dieRoll, newGen) = randomR (1, 6) gen :: (Int, StdGen)
  print dieRoll


tenRandomChars :: StdGen -> String
tenRandomChars gen = take 10 $ randomRs ('a', 'z') gen

-- main = do
--   gen <- getStdGen
--   putStr $ take 10 $ randomRs ('a', 'z') gen

main = do
  gen <- getStdGen
  rollDie' gen
