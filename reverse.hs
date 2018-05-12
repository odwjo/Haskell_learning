main = do
    line <- getLine
    if null line
        then return()
        else do 
            putStrLn $ reverseWords line
            main

-- do syntax glue together I/O actions
reverseWords :: String -> String
-- unwords = ["a", "b"] -> "a b"
-- words   = "a b" -> ["a", "b"]
reverseWords = unwords . map reverse . words
-- import Control.Monad when (Bool) $ do
-- sequence [getLine, getLine, getLine]
