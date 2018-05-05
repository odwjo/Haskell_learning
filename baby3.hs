import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

nub' :: (Eq a)=>[a]-> [a]
nub' [] = []
nub' (x:xs) = x : nub'(filter (/= x) xs)

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' xs = [xs] ++ (tails'. tail $ xs)

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool    
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys)
    |x == y = isPrefixOf' xs ys
    |otherwise = False 

any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs)
    |p x = True
    |otherwise = any' p xs

isIn' :: (Eq a) => [a] -> [a] -> Bool
isIn' x y = any' (x `isPrefixOf'`) (tails' y)



offencode :: Int -> String -> String
offencode off msg = map (\c -> chr $ ord c + off) msg
offdecode :: Int -> String -> String
offdecode off msg = map (\c -> chr $ ord c - off) msg

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

find' :: (a -> Bool) -> [a] -> Maybe a
find' _ [] = Nothing
find' p (x:xs)
    |p x = Just x
    |otherwise = find' p xs

firstTo :: Int -> Maybe Int
firstTo n = find(\x -> digitSum x == n) [1..]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findkey key ((k, v):xs)
    |k == key = Just v
    |otherwise = findkey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

