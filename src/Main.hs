import Control.Monad.State.Strict
import System.Environment
import Data.Functor.Identity
import Data.List
data OP_TYPE = 
    OP_PUSH | 
    OP_PLUS |
    OP_MINUS | 
    OP_DUMP
    deriving (Enum)
type OP = (OP_TYPE, Integer)
type Program = [OP]

push :: Integer -> OP
push x = (OP_PUSH, x)
plus :: OP
plus = (OP_PLUS, 0)
minus :: OP
minus = (OP_MINUS, 0)
dump :: OP
dump = (OP_DUMP, 0)



program :: Program
program = [
    push 34, 
    push 35, 
    plus, 
    dump,
    push 500,
    push 80,
    minus,
    dump
    ]

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

simulate :: Program -> StateT [Integer] IO ()
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
        f (OP_DUMP, _) = dumpSim

iostrFromFile :: FilePath -> IO String
iostrFromFile f = do
    readFile f
    

main :: IO ()
main = do
    args <- getArgs
    result <- runStateT (simulate program) []
    o <- iostrFromFile $ head args
    let 
        repl '\n' = ' '
        repl c = c
    print $ map repl o