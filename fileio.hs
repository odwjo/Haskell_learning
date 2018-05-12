import Data.Char
main = interact respondPalindromes
--do 
--    contents <- getContents
----    putStr $ map toUpper contents
--    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPal xs 
                                        then xs ++ " :palindrome"
                                        else "no palindromes") .
                                            lines
isPal :: String -> Bool
isPal xs = xs == reverse xs


