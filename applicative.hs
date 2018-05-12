import Control.Applicative

--class (Functor f) => Applicative f where
--  pure :: a -> f a
--  (<*>) :: f (a -> b) -> f a -> f b
--  (<$>) :: (Functor f) => (a -> b) -> f a -> f b
--  f <$> x = fmap f x

-- f take one concrete type as its parameter
-- just like Maybe

-- instance Applicative Maybe where
--  pure = Just
--  Nothing <*> _ = Nothing
--  (Just f) <*> something = fmap f something

-- instance Applicative [] where
--  pure x = [x]
--  fs <*> xs = [f x| f <- fs, x <- xs]

-- instance Applicative IO where
--  pure = return 
--  a <*> b = do 
--      f <- a
--      x <- b
--      return (f x)

-- instance Applicative ((->) r) where
--  pure x = (\_ -> x)
--  f <*> g = \x -> f x (g x)

-- instance Applicative ZipList where
--  pure x = ZipList (repeat x)
--  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
--  (,,) = \x y z -> (x,y,z)

--extract -> combine -> repack
--sequenceA :: (Applicative f) => t (f a) -> f (t a)
--sequenceA [] = pure []
--sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
--extract a from every element of [f a]
--sequenceA = foldr (liftA2 (:)) (pure []) 
--class Functor f where
--  fmap :: (a -> b) -> f a -> f b
-- the f a in declearation make the contents in f >>naked<<
lA2 ::(Applicative f) => (a -> b -> c) -> f a -> f b -> f c
lA2 f a b = f <$> a <*> b
--fmap declearation make [2,3,4] become [+2,+3,+4] <*> makes it out
--fmap (+) [2,3,4] <*>  [40,50,60]
--42,52,62,43,53,63,44,54,64]

--f is inside [],not [] itself
seqA :: (Applicative f) => [f a] -> f [a]
seqA = foldr (liftA2 (:) ) (pure [])
