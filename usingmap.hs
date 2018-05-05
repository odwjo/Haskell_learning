import qualified Data.Map as Map

people = [("Jones", "123"),("Tom", "356"),("Jerry", "9564"),("Tom", "233")]

multimap :: (Ord k) =>[(k, a)] -> Map.Map k [a]
multimap xs = Map.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs

