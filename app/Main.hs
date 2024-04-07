import System.Environment (getArgs)
import TextProcessor

hsDebug :: String -> IO ()
hsDebug source = print $ readProg source

help :: IO ()
help = print "fg-bytegen [gen|debug|help] <args>"

badCommand :: String -> IO ()
badCommand cmd = print $ "Bad command " ++ cmd

runGen :: [String] -> IO ()
runGen args = print $ "gen " ++ show args

runOp :: String -> [String] -> IO ()
runOp action args = do
    case action of
        "debug" -> hsDebug (unwords args)
        "gen" -> runGen args
        _ -> badCommand (concat args)

main :: IO ()
main = do
    cliArgs <- getArgs
    case cliArgs of
        [] -> help
        [c] -> if c == "-h" || c == "--help" || c == "help" then help else badCommand c
        (action:args) -> runOp action args