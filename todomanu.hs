import System.IO
import System.Directory
import Data.List
import Control.Exception

main = do 
    contents <- readFile "todo.txt"
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your To-Do items:"
    mapM_ putStrLn numberedTasks
    putStrLn "You want to add(a) or delete(d)?"
    op <- getLine
    if 'a' == (op !! 0)
        then do
            putStrLn "Please Input:"
            item <- getLine
            appendFile "todo.txt" (item ++ "\n")
        else do
            putStrLn "Which one do you want to delete?"
            numberString <- getLine
            let number = read numberString
                newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
            bracketOnError ( openTempFile "." "temp")
                (\(tempName, tempHandle) -> do
                    hClose tempHandle
                    removeFile tempName)
                (\(tempName, tempHandle) -> do
                    hPutStr tempHandle newTodoItems
                    hClose tempHandle
                    removeFile "todo.txt"
                    renameFile tempName "todo.txt")
    do
        putStrLn "Your list is updated now:"
        contents <- readFile "todo.txt"
        let
            todoItem = zipWith (\n line -> show n ++ " - " ++ line) [0..] $  lines contents
        mapM_ putStrLn todoItem

