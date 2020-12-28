-- |
-- Module      :  Data.MinMax
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions to find both minimum and maximum elements of the 'F.Foldable' structure of the 'Ord'ered elements.

module Data.MinMax where

import Prelude hiding (takeWhile,dropWhile,span)
import Data.SubG
import qualified Data.Foldable as F
import qualified Data.List as L (sortBy)

-- | Returns a pair where the first element is the minimum element from the two given ones and the second one is the maximum. If the arguments are
-- equal then the tuple contains equal elements.
minmaxP :: (Ord a) => a -> a -> (a,a)
minmaxP = minmaxPBy compare
{-# INLINE minmaxP #-}

-- | A variant of the 'minmaxP' where you can specify your own comparison function.
minmaxPBy :: (Ord a) => (a -> a -> Ordering) -> a -> a -> (a,a)
minmaxPBy g x y
 | g x y == LT = (x,y)
 | otherwise = (y,x)

-- | A ternary predicate to check whether the third argument lies between the first two unequal ones or whether they are all equal.
betweenNX :: (Ord a) => a -> a -> a -> Bool
betweenNX = betweenNXBy compare
{-# INLINE betweenNX #-}

-- | A variant of the 'betweenNX' where you can specify your own comparison function.
betweenNXBy :: (Ord a) => (a -> a -> Ordering) -> a -> a -> a -> Bool
betweenNXBy g x y z
 | x == y = x == z
 | g z k == LT && g z t == GT = True
 | otherwise = False
      where (t,k) = minmaxPBy g x y

-- | Finds out the minimum and maximum values of the finite structure that has not less than two elements. Otherwise returns 'Nothing'.
minMax11 :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> Maybe (a, a)
minMax11 = minMax11By compare
{-# INLINE minMax11 #-}

-- | A generalized variant of the 'minMax11' where you can specify your own comparison function.
minMax11By :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> Maybe (a, a)
minMax11By g xs
 | F.length xs < 2 = Nothing
 | otherwise = Just . F.foldr f (t,u) $ str1
      where (str1,str2) = splitAtEndG 2 $ xs
            [t,u] = L.sortBy g . F.toList $ str2
            f z (x,y)
              | g z x == LT = (z,y)
              | g z y == GT = (x,z)
              | otherwise = (x,y)

-- | Given a finite structure with at least 3 elements returns a tuple with the two most minimum elements
-- (the first one is less than the second one) and the maximum element. If the structure has less elements, returns 'Nothing'.
-- Uses just three passes through the structure, so may be more efficient than some other approaches.
minMax21 :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> Maybe ((a,a), a)
minMax21 = minMax21By compare
{-# INLINE minMax21 #-}

-- | A variant of the 'minMax21' where you can specify your own comparison function.
minMax21By :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> Maybe ((a,a), a)
minMax21By g xs
 | F.length xs < 3 = Nothing
 | otherwise = Just . F.foldr f ((n,p),q) $ str1
      where (str1,str2) = splitAtEndG 3 xs
            [n,p,q] = L.sortBy g . F.toList $ str2
            f z ((x,y),t)
              | g z t == GT = ((x,y),z)
              | g z y == LT = if g z x == GT then ((x,z),t) else ((z,x),t)
              | otherwise = ((x,y),t)

-- | Given a finite structure with at least 3 elements returns a tuple with the minimum element
-- and two maximum elements (the first one is less than the second one). If the structure has less elements, returns 'Nothing'.
-- Uses just three passes through the structure, so may be more efficient than some other approaches.
minMax12 :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> Maybe (a, (a,a))
minMax12 = minMax12By compare
{-# INLINE minMax12 #-}

-- | A variant of the 'minMax12' where you can specify your own comparison function.
minMax12By :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> Maybe (a, (a,a))
minMax12By g xs
 | F.length xs < 3 = Nothing
 | otherwise = Just . F.foldr f (n,(p,q)) $ str1
      where (str1,str2) = splitAtEndG 3 xs
            [n,p,q] = L.sortBy g . F.toList $ str2
            f z (x,(y,t))
              | g z x == LT = (z,(y,t))
              | g z y == GT = if g z t == LT then (x,(z,t)) else (x,(t,z))
              | otherwise = (x,(y,t))

-- | Given a finite structure with at least 4 elements returns a tuple with two minimum elements
-- and two maximum elements. If the structure has less elements, returns 'Nothing'.
-- Uses just three passes through the structure, so may be more efficient than some other approaches.
minMax22 :: (Ord a, InsertLeft t a, Monoid (t a)) => t a -> Maybe ((a,a), (a,a))
minMax22 = minMax22By compare
{-# INLINE minMax22 #-}

-- | A variant of the 'minMax22' where you can specify your own comparison function.
minMax22By :: (Ord a, InsertLeft t a, Monoid (t a)) => (a -> a -> Ordering) -> t a -> Maybe ((a,a), (a,a))
minMax22By g xs
 | F.length xs < 4 = Nothing
 | otherwise = Just . F.foldr f ((n,p),(q,r)) $ str1
      where (str1,str2) = splitAtEndG 4 xs
            [n,p,q,r] = L.sortBy g . F.toList $ str2
            f z ((x,y),(t,w))
              | g z y == LT = if g z x == GT then ((x,z),(t,w)) else ((z,x),(t,w))
              | g z t == GT = if g z w == LT then ((x,y),(z,w)) else ((x,y),(w,z))
              | otherwise = ((x,y),(t,w))
