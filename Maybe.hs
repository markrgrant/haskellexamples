-- A working example of the use of the Maybe Monad to support operations
-- on parent-child relationships where parent information could be missing.
-- This example is based on the examples provided in the Haskell Wikibook:
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe
module Main where

import Control.Monad ((>=>))


data Person = Person {
  name :: String,
  father :: Maybe Person,
  mother :: Maybe Person
} 


instance Show Person where
  show p = "[name=" ++ name p ++ " mother=" ++ 
    maybe "unknown" name (mother p) ++ " father=" ++ 
    maybe "unknown" name (father p) ++ "]"


-- use monadic composition
maternalGrandfather = mother >=> father
paternalGrandfather = father >=> father


bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p =
  paternalGrandfather p >>= \pgf ->
  maternalGrandfather p >>= \mgf ->
  return (pgf, mgf) 


main :: IO ()
main = print $ bothGrandfathers me
  where pgf = Person "fred"  Nothing Nothing
        mgf = Person "ed"    Nothing Nothing
        mgm = Person "wilma" Nothing Nothing
        mo  = Person "judy"  (Just mgf) (Just mgm)
        fa  = Person "bill"  (Just pgf) Nothing
        me  = Person "me"    (Just fa) (Just mo)
