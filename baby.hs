flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

mult :: (Num a) => a -> a -> a
mult a b = a * b

quicksort ::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort [ele|ele <- xs, ele <= x] ++
				[x] ++ 
				quicksort [ele | ele <- xs, ele > x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _[] = []
zip' []_ = []
zip' (x:xs)(y:ys) = (x,y):zip' xs ys

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _[] = []
take' n (x:xs) = x : take' (n-1) xs

replicate' :: Int -> a -> [a]
replicate' n x
	|n <= 0 = []
	|otherwise = x : replicate' (n-1) x

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

calcBmis:: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w,h) <- xs]
	where bmi wt ht = wt/ht^2


myCompare :: (Ord a) => a->a->Ordering
a `myCompare` b
	|a == b = EQ
	|a <= b = LT
	|otherwise = GT

bmiTell :: Double -> Double -> String
bmiTell weight height
	| bmi <= 18.5 = "You are thin."
	| bmi <= 25.0 = "You are normal."
	| bmi <= 30.0 = "You are fat."
	| otherwise = "Maybe a little bit of too overweight."
	where bmi = weight/height^2
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
						then x
						else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

lucky::Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "sorry, you're out of luck, pal!"
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors    (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Cna't call head on an empty list."
head' (x:_) = x

tell:: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one emlement: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. " ++ show x ++ " and " ++ show y ++ " ..."
