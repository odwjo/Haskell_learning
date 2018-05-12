import Control.Monad.Trans.Writer

-- data ZipList a = ZipList {getZipList :: [a]}
--the first a means that this ZipList is about to wrap this type
--whick is deduced from the [a] parameter

newtype Pair b a = Pair { getPair :: (a, b)}
instance Functor (Pair c) where
    fmap f (Pair(x, y)) = Pair(f x, y)

newtype Product a = Product {getProduct :: a}
    deriving(Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

newtype Sum a = Sum {getSum :: a}
    deriving(Eq, Ord, Read, Show, Bounded)
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum a `mappend` Sum b = Sum (a + b)


newtype Any = Any {getAny :: Bool}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
    mempty = Any False
    Any x `mappend` Any y = Any (x || y)

newtype All = All {getAll :: Bool}
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
    mempty = All True
    All x `mappend` All y = All (x && y)

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
                    (x `compare` y)

lengthCompare' ::String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend`
                     (vowels x `compare` vowels y) `mappend`
                     (x `compare` y)
                     where vowels = length . filter (`elem` "aeiou")
newtype First a = First {getFirst :: Maybe a}
    deriving (Eq, Ord, Read, Show)

instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x

--newtype Writer w a = Writer {runWriter :: (a,w)}
--instance (Monoid w) => Monad (Writer w) where
--    return x = Writer (x ,mempty)
--    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y,v `mappend` v')

longNumber :: Int -> Writer [String] Int
longNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do 
    a <- longNumber 3
    b <- longNumber 5
    tell ["Multiply ..."]
    return (a*b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finish with " ++ show a]
        return a
    | otherwise = do
        tell[show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []
instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f(g xs))

gcd2 :: Int -> Int -> Writer (DiffList String) Int
gcd2 a b
    |b == 0 = do
        tell (toDiffList ["Finished with "++ show a])
        return a
    |otherwise = do
        result <- gcd2 b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show(a `mod` b)])
        return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])


