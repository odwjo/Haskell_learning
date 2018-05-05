chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    |odd n = n:chain (n*3 + 1)
    |even n = n:chain (n `div` 2)

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x:acc) []

reverse2' :: [a] -> [a]
reverse2' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) +1


