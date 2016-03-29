-- An example of the use of Maybe as an instance of MonadPlus.  This
-- instance provides  four functions that can be used to provide additional
-- functionality:
--
-- a computation that can be combined with any other computation 
-- such that the other computation's value is not changed
-- mzero :: MonadPlus a => m a
-- 
-- combine individual computations in parallel to create a new
-- computation
-- mplus :: MonadPlus a => m a -> m a -> m a
--
-- uses mplus to combine a list of computations into a result
-- msum :: MonadPlus a => [m a] -> m a
--
-- takes a boolean value and returns either mzero (if it is false) or
-- 
-- guard :: MonadPlus m => Bool -> m ()
--
-- The example is taken from:
-- https://en.wikibooks.org/wiki/Haskell/MonadPlus-


-- an example using the MonadPlus instance of Maybe.
import Control.Monad
import Data.Maybe (fromJust)


digit :: Int -> String -> Maybe Int
digit i s 
  | i > 9 || i < 0 = Nothing
  | otherwise = let (c:_) = s in if [c] == show i then Just i else Nothing


-- use the digit parser above to create a new parser that parses either 0's
-- or 1's in a string
binChar :: String -> Maybe Int
binChar [] = Nothing
binChar s = digit 0 s `mplus` digit 1 s



-- An example using the MonadPlus instance of List.
pythags = [1..] >>= \z -> 
          [1..z] >>= \x ->
          [x..z] >>= \y ->
          guard (x^2 + y^2 == z^2) >>
          return (x,y,z)


main :: IO ()
main = putStrLn ("first digit   = " ++ show (binChar "010")) >>
       putStrLn ("first triplet = " ++ show (head pythags))
