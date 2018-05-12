import Data.List

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (y * x):ys
          foldingFunction (x:y:ys) "+" = (y + x):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction (x:y:ys) "^" = (y ** x):ys
          foldingFunction (x:xs) "ln" = log x:xs
          foldingFunction xs "sum" = [sum xs]
          foldingFunction xs numberString = read numberString:xs
-- foldingFunction ::String -> String -> String
data Section = Section { getA :: Int, getB :: Int, getC :: Int}
    deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                    ,Section 5 90 20
                    ,Section 40 2 25
                    ,Section 10 8 0
                    ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (patha, pathb) (Section a b c) = 
    let timeA = sum (map snd patha)
        timeB = sum (map snd pathb)
        forwardTimeToA = timeA + a
        crossTimeToA    = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB    = timeA + a + c
        newPathToA = if forwardTimeToA <= crossTimeToA
                        then (A, a):patha
                        else (C, c):(B, b):pathb
        newPathToB = if forwardTimeToB <= crossTimeToB
                        then (B, b):pathb
                        else (C, c):(A, a):patha
    in (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do 
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathTime = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime
