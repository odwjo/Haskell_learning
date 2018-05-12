import Data.List
import Control.Monad
import Control.Monad.Trans.State
import System.Random
import Control.Monad.Trans.Writer
--import Control.Monad.Instances
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- class Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> (a -> m b) -> m b

--  (>>) :: m a -> m b -> m b
--  x >> y = x >>= \_ -> y

--  fail :: String -> m a
--  fail msg = error msg

--instance Monad Maybe where
--  return x = Just x
--  Nothing >>= f = Nothing
--  Just x >>= f = f x
--      f x must return a Maybe value
--  fail _ = Nothing


type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left+n)-right) < 4 = Just (left +n, right)
    | otherwise = Nothing
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs ((right+n)-left) < 4 = Just (left, right + n)
    | otherwise = Nothing
banana :: Pole -> Maybe Pole
banana _ = Nothing
foo :: Maybe String
--foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)
routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landLeft 1 second
--instance Monad [] where
--    return x = [x]               -- mappend = ++
--  if xs is empty, then f has no ele to map,so result is void ,
--  especially in `>>` operate 
--    xs >>= f = concat (map f xs) 
--  f :: a -> m b
--  mconcat = foldr mappend mempty
--    fail _ = []
-- [1,2] is mapped by lambda
--[1,2] >>= \n -> ['a', 'b'] >>= \ch ->  return (n, ch)
--[1,2] >>= (\n -> ['a', 'b'] >>= (\ch ->  return (n, ch)))
listOfTuples = do
    n <- [1,2]
    ch <- ['a','a']
    return (n, ch)

--class Monad m => MonadPlus m where
--  mzero :: m a
--  mplus :: m a -> m a -> m a
--instance MonadPlus [] where
--  mzero = []
--  mplus = (++)
--guard :: (MonadPlus m) => Bool -> m ()
--guard True = return --no empty value
--guard False = mzero

type KnightPos = (Int, Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r+1),(c-2,r-1)
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
--return [1,2,3] >>= (\x -> [x,x,x]) gen [[1,2,3],[1,2,3],[1,2,3]]
--return wrap the whole [1,2,3] in a elem
-- == (\x -> [x.x.x]) [1,2,3]
-- Just "xx" >> (\x -> return x) == Just "xx"

--(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
--f <=< g = (\x -> g x >>= f)

-- f <<= (g <=< h) == (f <=< g) <=< h
-- f == f <=< return
-- f == return <=< f

--Control.Monad.Instances
--instance Monad ((->) r) where
--    return x =\_ -> x
--    h >>= f = \w -> f (h w) w
------actually f takes two values
------f is a function which returns a function 

addStuff :: Int -> Int
addStuff = do
    a <- (*2) -- read Monad
    b <- (+10)
    return (a+b)

addStuff2 :: Int -> Int
addStuff2 x = let
    a = (*2) x
    b = (+10) x
    in a + b

------state Monad
type Stack = [Int]
--pop :: Stack -> (Int, Stack)
--pop (x:xs) = (x, xs)
--push :: Int -> Stack -> ((), Stack)
--push a xs = ((), a:xs)

--stackManip :: Stack -> (Int, Stack)
--stackManip stack = let
--    ((), newStack1) = push 3 stack
--    (a , newSt:ack2) = pop newStack1
--    in pop newStack2

----defined in Control.Monad.State
----so state is wraping a function

----State s a ::  State Stack Int or State Stack ()
--newtype State s a = State {runState :: s -> (a, s) }
--instance Monad (State s) where
--    return x = State $ \s -> (x,s)
--    (>>=) :: State s a -> (a->State s b) -> State s b
--    (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                        (State g) = f a
--                                    in g newState
--get = state $ \s -> (s, s)
--put newState = state $ \s -> ((), newState)

pop :: State Stack Int
--pop = state $ \(x:xs) -> (x,xs)
pop = do
    (x:xs) <- get
    put xs
    return x
push :: Int -> State Stack ()
--push a = state $ \xs -> ((),a:xs)
push x = do
    xs <- get
    put (x:xs)


stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop --a extract the x of (x,xs) of state
    pop

randomSt :: (RandomGen g,Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins  = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)
{-
liftM :: (Monad m) => (a -> b) -> m a -> m b
--liftM f m = m >>= (\x -> return (f x))
liftM f m = do
    x <- m
    return (f x)
join :: (Monad m) m (m a) -> m a
join mm = do
    m <- mm
    m
--}
--filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    |x < 4 = do
        tell ["Keeping "++ show x]
        return True
    |otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

powerset ::  [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
--foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((y * x):ys)
foldingFunction (x:y:ys) "+" = return ((y + x):ys)
foldingFunction (x:y:ys) "-" = return ((y - x):ys)
foldingFunction (x:y:ys) "/" = return ((y / x):ys)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

