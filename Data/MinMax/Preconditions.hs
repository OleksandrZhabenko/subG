-- |
-- Module      :  Data.MinMax.Preconditions
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions to find both minimum and maximum elements of the 'F.Foldable' structure of the 'Ord'ered elements. With the preconditions that the
-- structure at least have enough elements (this is contrary to the functions from the module Data.MinMax not checked internally).

module Data.MinMax.Preconditions where

import Prelude hiding (takeWhile,dropWhile,span)
import Data.SubG
import qualified Data.Foldable as F
import qualified Data.List as L (sortBy)

-- | Finds out the minimum and maximum values of the finite structure that has not less than two elements.
minMax11C :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> (a, a)
minMax11C = minMax11ByC compare
{-# INLINE minMax11C #-}

-- | A generalized variant of the 'minMax' where you can specify your own comparison function.
minMax11ByC :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> (a, a)
minMax11ByC g xs =
  F.foldr f (t,u) str1
    where (str1,str2) = splitAtEndG 2 $ xs
          [t,u] = L.sortBy g . F.toList $ str2
          f z (x,y)
            | g z x == LT = (z,y)
            | g z y == GT = (x,z)
            | otherwise = (x,y)

-- | Given a finite structure returns a tuple with the two most minimum elements
-- (the first one is less than the second one) and the maximum element.
-- Uses just two passes through the structure, so may be more efficient than some other approaches.
minMax21C :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> ((a,a), a)
minMax21C = minMax21ByC compare
{-# INLINE minMax21C #-}

-- | A variant of the 'minMax21C' where you can specify your own comparison function.
minMax21ByC :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> ((a,a), a)
minMax21ByC g xs =
  F.foldr f ((n,p),q) str1
    where (str1,str2) = splitAtEndG 3 xs
          [n,p,q] = L.sortBy g . F.toList $ str2
          f z ((x,y),t)
            | g z t == GT = ((x,y),z)
            | g z y == LT = if g z x == GT then ((x,z),t) else ((z,x),t)
            | otherwise = ((x,y),t)

-- | Given a finite structure returns a tuple with the minimum element
-- and two maximum elements (the first one is less than the second one).
-- Uses just two passes through the structure, so may be more efficient than some other approaches.
minMax12C :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> (a, (a,a))
minMax12C = minMax12ByC compare
{-# INLINE minMax12C #-}

-- | A variant of the 'minMax12C' where you can specify your own comparison function.
minMax12ByC :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> (a, (a,a))
minMax12ByC g xs =
  F.foldr f (n,(p,q)) $ str1
    where (str1,str2) = splitAtEndG 3 xs
          [n,p,q] = L.sortBy g . F.toList $ str2
          f z (x,(y,t))
            | g z x == LT = (z,(y,t))
            | g z y == GT = if g z t == LT then (x,(z,t)) else (x,(t,z))
            | otherwise = (x,(y,t))

-- | Given a finite structure returns a tuple with two minimum elements
-- and two maximum elements. Uses just two passes through the structure, so may be more efficient than some other approaches.
minMax22C :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> ((a,a), (a,a))
minMax22C = minMax22ByC compare
{-# INLINE minMax22C #-}

-- | A variant of the 'minMax22C' where you can specify your own comparison function.
minMax22ByC :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> ((a,a), (a,a))
minMax22ByC g xs =
  F.foldr f ((n,p),(q,r)) $ str1
    where (str1,str2) = splitAtEndG 4 xs
          [n,p,q,r] = L.sortBy g . F.toList $ str2
          f z ((x,y),(t,w))
            | g z y == LT = if g z x == GT then ((x,z),(t,w)) else ((z,x),(t,w))
            | g z t == GT = if g z w == LT then ((x,y),(z,w)) else ((x,y),(w,z))
            | otherwise = ((x,y),(t,w))
