import System.IO
main = do
        putStrLn "Which file you want to open (to read)?"
        filename <- getLine
        withFile filename ReadMode (\handle -> do 
                            contents <- hGetContents handle
                            putStr contents)
--        handle <- openFile filename ReadMode
--        contents <- hGetContents handle
--        putStr contents
--        hClose handle
-- bracket :: IO resource -> (resource -> IO close) -> (resource -> IO function) -> IO function
--withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
--withFile name mode f = bracket (openFile name mode) (\handle -> hClose handle) (\handle ->f handle)
--hGetLine, hPutStr, hPutStrLn, hGetChar
--readFile, writeFile, appendFile



