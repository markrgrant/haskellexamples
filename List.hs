-- Examples using lists as Monads that model nondeterministic
-- computation.  In this example, the nondeterministic computation
-- is the set of possible locations that a person could be on a map
-- after taking arbitrary turns.   The example is from 
module Main where


move :: Int -> [Int]
move cur = [cur-1,cur,cur+1]


-- move 3 times.  The result is a list of possible positions
-- after 3 moves.  The frequency of a particular position can be 
-- used to calculate its probability as all outcomes are preserved.
main :: IO ()
main = print $ move 0 >>= move >>= move
