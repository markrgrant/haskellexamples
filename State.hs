-- An example of using the State Monad to maintain a random number
-- seed value that is part of the context.  This seed value is used
-- to ensure that the next random value retrieved is provided with 
-- a unique seed value.
--
-- Although there are functions defined in System.Random that operate
-- within the IO monad and handle the seed state for us, they require
-- operating within the IO Monad.
import System.Random (mkStdGen, StdGen, randomR)
import Control.Monad (liftM2, replicateM)
import Control.Monad.Trans.State


dieRoll :: State StdGen Int
dieRoll = state $ randomR (1,6)


pairs :: State StdGen (Int, Int)
pairs = liftM2 (,) dieRoll dieRoll


rollNDice :: Int -> State StdGen [Int]
rollNDice n = replicateM n dieRoll


main :: IO ()
main = print $ evalState pairs (mkStdGen 0)
