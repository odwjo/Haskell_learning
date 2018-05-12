import System.IO        
import System.Directory --"."
import Data.List        --delete
import Control.Exception --bracketOnError
import System.Environment --getArgs, getProgName

dispatch :: String -> [String] -> IO()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatck command = doesntExist command

doesntExist :: String -> [String] -> IO()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist."

main = do
    (command:argList) <- getArgs
    dispatch command argList

add :: [String] -> IO()
add [filename, todoItem] = appendFile filename (todoItem ++ "/n")

view :: [String] -> IO()
view [filename] = do
    contents <- readFile filename
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO()
remove [filename, numberString] = do
    contents <- readFile filename
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "There are your To-Do items:"
    mapM_ putStrLn numberedTasks
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)

bump :: [String] -> IO()
bump [filename, numberString] = do
    contents <- readFile filename
    let todoTasks = lines contents
        index = read numberString
        item = todoTasks !! index
        partTasks = delete (todoTasks !! index) todoTasks
        newTodoTasks = item : partTasks
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] newTodoTasks
        newTodoItems = unlines newTodoTasks
    putStrLn "You file is like this now:"
    mapM_ putStrLn numberedTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile filename
            renameFile tempName filename)
