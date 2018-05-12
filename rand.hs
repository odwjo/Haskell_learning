import System.Random

boolGen :: StdGen -> Int -> [Bool]
boolGen gen i  
    | i <= 0 = []
    |otherwise = s : boolGen gen' j
        where j = i - 1
              (s, gen') = random gen
count :: (Eq a) => [a] -> a -> Int
count [] _ = 0
count (x:xs) a 
    | x == a = 1 + count xs a
    | x /= a = count xs a

--main = do
--  gen <- getStdGen
--  putStrLn $ take 20 (randomRs ('a', 'z') gen)

 
