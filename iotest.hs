import Data.Char
main = do
    putStrLn "Hello, what's your first name?"
    -- Can not be getLine -> name
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn $ "Hey " ++ bigFirstName ++ " " ++
                bigLastName ++ ", how are you doing?"
