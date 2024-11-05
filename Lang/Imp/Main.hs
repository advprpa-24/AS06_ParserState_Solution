module Main where
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


-- In this solution IO and pure code are mixed
-- There are nested case statements as well.
-- We could use the Either monad to clean this up.
-- Look at Main_Variant.hs for a cleaner solution.
runCode :: FilePath -> State -> IO ()
runCode file state = do
    content <- readFile file

    let ast = parseImp content
    case ast of
        Left parseError -> do
            putStrLn "Parse Error:"
            putStrLn parseError
        Right cmd -> do
            putStrLn "Input State:"
            putStrLn $ prettyState state
            case runState (exec cmd) state of
                Left intError -> do
                    putStrLn "Runtime Error:"
                    putStrLn intError
                Right (resState, _) -> do
                    putStrLn "Result State:"
                    putStrLn $ prettyState resState

prettyState :: State -> String
prettyState m = intercalate "\n" (map (\(k,v) -> show k ++ " -> " ++ show v) (M.toList m))

main :: IO ()
main = do
    args <- getArgs
    dispatch $ interpretArgs args
