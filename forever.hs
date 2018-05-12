import Control.Monad
import Data.Char
--forever dangerous Ctrl+D is the cure
--main = forever $ do
--    putStr "Give me some input: "
--    l <- getLine
--    putStrLn $ map toUpper l

main = do 
    colors <- forM [1..4] (\a -> do
        putStrLn $ "Which color do you associate with the number "
            ++ show a ++ " ?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors    
