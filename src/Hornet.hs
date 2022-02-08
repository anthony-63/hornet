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
    OP_END 
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

length' :: (Num b) => [a] -> b 
length' [] = 0 
length' xs = sum [1 | _ <- xs]

simulateOP :: OP -> Int -> StateT [Integer] IO ()
simulateOP (OP_PUSH, x) _ = pushSim x
simulateOP (OP_PLUS, _) _ = do
    a <- popSim
    b <- popSim
    pushSim(a + b)
simulateOP (OP_MINUS, _) _ = do
    b <- popSim
    a <- popSim
    pushSim(a - b)
simulateOP (OP_MULTIPLY, _) _ = do
    b <- popSim
    a <- popSim
    pushSim(a * b)
simulateOP (OP_DIVIDE, _) _ = do
    b <- popSim
    a <- popSim
    pushSim(quot a b)
simulateOP (OP_DUP, _) _ = do 
    a <- popSim
    pushSim a
    pushSim a
simulateOP (OP_DUMP, _) _ = dumpSim
simulateOP (OP_DUMPALL, _) _ = dumpAllSim
simulateOP (OP_SWAP, _) _ = do
    a <- popSim
    b <- popSim
    pushSim a
    pushSim b
simulateOP (OP_GT, _) _ = do
    b <- popSim
    a <- popSim
    pushSim $ checkBool (a > b)
simulateOP (OP_LT, _) _ = do
    a <- popSim
    b <- popSim
    pushSim $ checkBool (a < b)
simulateOP (OP_EQ, _) _ = do
    b <- popSim
    a <- popSim
    pushSim $ checkBool (a == b)
simulateOP (OP_END, _) _ = 
    pushSim 0
simulate :: Program -> Int -> StateT [Integer] IO () --simulation
simulate x y = do
    if y < (length x) then do
        simulateOP (x !! y) y
        simulate x $ y + 1
    else simulateOP (OP_END, 0) y
 
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
    let a = parseStr o
    result <- runStateT (simulate a 0) []
    putStr ""
