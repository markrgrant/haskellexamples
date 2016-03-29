-- Example of the use of the MaybeT Monad Transformer as
-- presented in the Haskell Wikibook: 
-- https://en.wikibooks.org/wiki/Haskell/Monad_transformers

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import Data.Char

isValid :: String -> Bool
isValid s = length s >= 8 &&
  any isAlpha s &&
  any isNumber s &&
  any isPunctuation s


getValidPassphrase :: MaybeT IO String
getValidPassphrase = lift getLine >>= \pass -> 
                     guard (isValid pass) >>
                     return pass


askPassphrase :: MaybeT IO ()
askPassphrase = lift (putStrLn "Insert your new passphrase: ") >>
                msum (repeat getValidPassphrase) >>
                lift (putStrLn "String in database ...")


main :: IO ()
main = void (runMaybeT askPassphrase)
