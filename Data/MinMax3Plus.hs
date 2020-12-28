-- |
-- Module      :  Data.MinMax3Plus
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions to find both minimum and maximum elements of the 'F.Foldable' structure of the 'Ord'ered elements.

module Data.MinMax3Plus where

import Prelude hiding (takeWhile,dropWhile,span)
import Data.SubG
import qualified Data.Foldable as F
import qualified Data.List as L (sortBy)

-- | Given a finite structure returns a tuple with two minimum elements
-- and three maximum elements.
-- Uses just three passes through the structure, so may be more efficient than some other approaches.
minMax23 :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> ((a,a), (a,a,a))
minMax23 = minMax23By compare
{-# INLINE minMax23 #-}

-- | A variant of the 'minMax23' where you can specify your own comparison function.
minMax23By :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> ((a,a), (a,a,a))
minMax23By g xs =
  F.foldr f ((n,p),(q,r,s)) $ str1
    where (str1,str2) = splitAtEndG 5 xs
          [n,p,q,r,s] = L.sortBy g . F.toList $ str2
          f z ((x,y),(t,w,u))
            | g z y == LT = if g z x == GT then ((x,z),(t,w,u)) else ((z,x),(t,w,u))
            | g z t == GT = if g z w == LT then ((x,y),(z,w,u)) else if g z u == LT then ((x,y),(w,z,u)) else ((x,y),(t,w,u))
            | otherwise = ((x,y),(t,w,u))

-- | Given a finite structure returns a tuple with three minimum elements
-- and two maximum elements. Uses just three passes through the structure, so may be more efficient than some other approaches.
minMax32 :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> ((a,a,a), (a,a))
minMax32 = minMax32By compare
{-# INLINE minMax32 #-}

-- | A variant of the 'minMax32' where you can specify your own comparison function.
minMax32By :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> ((a,a,a), (a,a))
minMax32By g xs =
  F.foldr f ((n,m,p),(q,r)) $ str1
    where (str1,str2) = splitAtEndG 5 xs
          [n,m,p,q,r] = L.sortBy g . F.toList $ str2
          f z ((x,y,u),(t,w))
            | g z u == LT = if g z y == GT then ((x,y,z),(t,w)) else if g z x == GT then ((x,z,y),(t,w)) else ((z,x,y),(t,w))
            | g z t == GT = if g z w == LT then ((x,y,u),(z,w)) else ((x,y,u),(w,z))
            | otherwise = ((x,y,u),(t,w))

-- | Given a finite structure returns a tuple with three minimum elements
-- and three maximum elements. Uses just three passes through the structure, so may be more efficient than some other approaches.
minMax33 :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> ((a,a,a), (a,a,a))
minMax33 = minMax33By compare
{-# INLINE minMax33 #-}

-- | A variant of the 'minMax33' where you can specify your own comparison function.
minMax33By :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> ((a,a,a), (a,a,a))
minMax33By g xs =
  F.foldr f ((n,m,p),(q,r,s)) $ str1
    where (str1,str2) = splitAtEndG 6 xs
          [n,m,p,q,r,s] = L.sortBy g . F.toList $ str2
          f z ((x,y,u),(t,w,k))
            | g z u == LT = if g z y == GT then ((x,y,z),(t,w,k)) else if g z x == GT then ((x,z,y),(t,w,k)) else ((z,x,y),(t,w,k))
            | g z t == GT = if g z w == LT then ((x,y,u),(z,w,k)) else if g z k == LT then ((x,y,u),(w,z,k)) else ((x,y,u),(w,k,z))
            | otherwise = ((x,y,u),(t,w,k))
