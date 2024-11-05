module Main_Variant where
import System.Environment (getArgs)
import Lang.Imp.Parser
import Lang.Imp.Ast
import Lang.Imp.Interpreter
import Lang.Imp.Pretty
import Data.List (intercalate)
import qualified Data.Map.Strict as M


data Action = Run FilePath State
            | Pretty FilePath
            | Help


interpretArgs :: [String] -> Action
interpretArgs []         = Help
interpretArgs ["--help"] = Help
interpretArgs ["--pretty", fileName] = Pretty fileName
interpretArgs (fileName:assignments) = Run fileName (parseAssignments assignments)


parseAssignments :: [String] -> State
parseAssignments = M.fromList . fmap parseAssignment 


-- String -> Either Error (Name, Integer) would be cleaner
parseAssignment :: String -> (Name, Integer)
parseAssignment str = (name, read v)
    where (name, _:v) = break (== '=') str


dispatch :: Action -> IO ()
dispatch Help = showHelpAction
dispatch (Pretty file) = formatFile file
dispatch (Run file state) = runCode file state


helpText :: String
helpText = concat [
     "imp runs code in the given file with the given arguments.\n",
     "Example usage:\n> imp example.imp a=12 b=13"
  ]


showHelpAction :: IO ()
showHelpAction = putStrLn helpText


formatFile :: FilePath -> IO ()
formatFile file = do
    content <- readFile file
    case parseImp content of
        Left parseError -> do
            putStrLn "Parse Error:"
            putStrLn parseError
        Right cmd -> do
            putStrLn $ ppCmd cmd

-- Here we load the content of the file (IO)
-- process the content (pure) in a separate function
-- and print the result (IO)
runCode :: FilePath -> State -> IO ()
runCode file state = do
    content <- readFile file
    case process content state of
        Left err -> do
            putStrLn "Error:"
            putStrLn err
        Right result -> do
            putStrLn "Result State:"
            putStrLn $ prettyState result 


-- This is a pure function now. Much better.
-- The next imprvement would be to replace the error message with 
-- a custom error type:
-- data Error = ParseError String | RuntimeError String
process :: String -> State -> Either String State
process content state = do
    cmd <- parseImp content
    (res, _) <- runState (exec cmd) state
    pure res


prettyState :: State -> String
prettyState m = intercalate "\n" (map (\(k,v) -> show k ++ " -> " ++ show v) (M.toList m))


main :: IO ()
main = do
    args <- getArgs
    dispatch $ interpretArgs args
