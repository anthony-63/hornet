import Control.Monad.State.Strict
import System.Environment
import Data.Functor.Identity
import Data.List
import System.Exit
import Control.Exception (throw)
data OP_TYPE = 
    OP_PUSH | 
    OP_PLUS |
    OP_MINUS | 
    OP_MULTIPLY |
    OP_DIVIDE |
    OP_DUP |
    OP_DUMP |
    OP_DUMPALL |
    OP_SWAP |
    OP_LT |
    OP_GT |
    OP_EQ |
    OP_ERR
    deriving (Enum)
type OP = (OP_TYPE, Integer)
type Program = [OP]



popSim :: StateT [Integer] IO Integer
popSim = do
    a <- get
    put $ tail a
    pure $ head a

pushSim :: Integer -> StateT [Integer] IO ()
pushSim x = do state <- get
               put $ x : state

dumpSim :: StateT [Integer] IO ()
dumpSim = do 
    a <- popSim
    liftIO $ print a

dumpAllSim :: StateT [Integer] IO ()
dumpAllSim = do 
    state <- get
    liftIO $ putStr ("[" ++ show (head state) ++ "] ")
    liftIO $ print $ reverse state

checkBool :: Bool -> Integer
checkBool True = 1
checkBool False = 0

simulate :: Program -> StateT [Integer] IO () --simulation
simulate = mapM_ f
    where
        f :: OP -> StateT [Integer] IO ()
        f (OP_PUSH, x) = pushSim x
        f (OP_PLUS, _) = do
            a <- popSim
            b <- popSim
            pushSim(a + b)
        f (OP_MINUS, _) = do
            b <- popSim
            a <- popSim
            pushSim(a - b)
        f (OP_MULTIPLY, _) = do
            b <- popSim
            a <- popSim
            pushSim(a * b)
        f (OP_DIVIDE, _) = do
            b <- popSim
            a <- popSim
            pushSim(quot a b)
        f (OP_DUP, _) = do 
            a <- popSim
            pushSim a
            pushSim a
        f (OP_DUMP, _) = dumpSim
        f (OP_DUMPALL, _) = dumpAllSim
        f (OP_SWAP, _) = do
            a <- popSim
            b <- popSim
            pushSim a
            pushSim b
        f (OP_GT, _) = do
            b <- popSim
            a <- popSim
            pushSim $ checkBool (a > b)
        f (OP_LT, _) = do
            b <- popSim
            a <- popSim
            pushSim $ checkBool (a < b)
        f (OP_EQ, _) = do
            a <- popSim
            b <- popSim
            pushSim $ checkBool (a == b)
        f (OP_ERR, _) = error "This should never be reached... OP_ERR"
            
iostrFromFile :: FilePath -> IO String
iostrFromFile f = do
    readFile f

isNumber :: String -> Bool
isNumber str =
    case reads str :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False


checkS :: String -> OP
checkS "+" = (OP_PLUS, 0)
checkS "-" = (OP_MINUS, 0)
checkS "*" = (OP_MULTIPLY, 0) 
checkS "/" = (OP_DIVIDE, 0)
checkS "dup" = (OP_DUP, 0)
checkS "dump" = (OP_DUMP, 0)
checkS "dumpall" = (OP_DUMPALL, 0)
checkS "swap" = (OP_SWAP, 0)
checkS ">" = (OP_LT, 0)
checkS "<" = (OP_GT, 0)
checkS "==" = (OP_EQ, 0)

checkS x = do
    if isNumber x then (OP_PUSH, read x :: Integer)
    else do
        errorWithoutStackTrace $ "Invalid word '" ++ x ++ "'"

parseStr :: String -> Program -- first pass
parseStr x = do
    let 
        repl '\n' = ' '
        repl c = c
    a <- words $ map repl x
    map checkS [a]


main :: IO ()
main = do
    args <- getArgs
    o <- iostrFromFile $ head args 
    result <- runStateT (simulate $ parseStr o) []
    putStr ""