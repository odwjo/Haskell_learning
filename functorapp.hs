import System.IO
--import Control.Monad.Instances
--instance Functor IO where
--    fmap f action = do
--        result <- action
--        return (f result)

main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backward!"
          putStrLn $ "Yes, you realy said " ++ line ++ " backwards!"


