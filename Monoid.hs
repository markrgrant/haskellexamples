-- Based on the must-read article:
-- http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
--
-- A monoid is a set with an associative binary operator that is closed on the
-- set, and a special element called zero that when combined with any other
-- element in the set using the operator yields the other element. 
--
-- In Haskell, monoids are represented by types that are instances of the 
-- Monoid typeclass. The following functions are defined for any Monoid
-- instance:
-- 'mempty' :: a -- the zero element of the monoid
-- 'mappend':: a -> a -> a -- the binary associative operator of the monoid
-- 'mconcat':: [a] -> a -- helper function that concatenates using mappend
--
-- there a convenient infix operator, named '<>', that can be used in
-- place of mappend when working with Monoid values. 
--
-- monoids can be thought of as accumulators, where we accmulate lists or
-- integer values. 
--
-- A type that is an instance of Monoid is useful because it ensures that
-- values can't be split up or decomposed into parts when the values are
-- passed.  Only further accumulation can occur.  When working with lists, for
-- example, if a function should not split up the list, then use Monoid
-- in the type signature so that values are treated as monoids. 
--
-- The Data.Monoid library defines the Monoid type class, and instances for
-- common types such as List, Integers (Sum and Product)  and Bools
-- (Any and All).
-- 
-- Monoids are compositional. For example, tuples whose elements are Monoids
-- are themselves Monoids with mempty containing the mempty value of each
-- member monoid, and mappend applying the mappend of each member monoid.  A
-- Monoid that is composed of other Monoids is called a "product" Monoid. 
--
-- Monoids are very useful when used as values contained in types that are
-- instances of Foldable (such as a tree).  Because the values are appendable,
-- foldMap can accumulate a final monoidal value using the monoid's own
-- mappend method.
--
-- The following example demonstrates how a tuple Monoid can be used to
-- accumulate multiple properties of a Foldable, just by providing a function
-- that translates individual values in the Foldable into members of a
-- monoid.  The accumulation is handled by the Monoid's definition.

import Data.Foldable  -- for the Foldable typeclass definition
import Data.Monoid    -- for the Monoid instances Any, and Sum


data Tree a = Tree a [Tree a]


instance Foldable Tree where
  foldMap f (Tree a subtrees) = f a <> mconcat (fmap (foldMap f) subtrees)


main = print results
  where tree    = Tree 3 [Tree 4 [Tree 0 []], Tree 5 []]
        results = foldMap (\x -> (Any (x == 0), Sum x)) tree
