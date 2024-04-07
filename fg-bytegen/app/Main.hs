import System.Environment (getArgs)
import TextProcessor

main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (readProg expr)