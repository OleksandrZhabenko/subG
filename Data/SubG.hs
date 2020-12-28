-- |
-- Module      :  Data.SubG
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Some extension to the 'F.Foldable' and 'Monoid' classes. Introduces a new class 'InsertLeft' -- the class of types of values that can be inserted from the left
-- to the 'F.Foldable' structure that is simultaneously the data that is also the 'Monoid' instance.

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.SubG (
  InsertLeft(..)
  , subG
  , takeG
  , takeFromEndG
  , reverseTakeG
  , reverseTakeFromEndG
  , dropG
  , dropFromEndG
  , reverseDropG
  , reverseDropFromEndG
  , takeWhile
  , dropWhile
  , span
  , splitAtG
  , splitAtEndG
  , preAppend
  , safeHeadG
  , safeTailG
  , safeInitG
  , safeLastG
  , mapG
  , filterG
  , partitionG
) where

import Prelude hiding (dropWhile, span, takeWhile)
import qualified Data.Foldable as F
import Data.Monoid

infixr 1 %@, %^

-- | Some extension to the 'F.Foldable' and 'Monoid' classes.
class (F.Foldable t, Eq a, Eq (t a)) => InsertLeft t a where
  (%@) :: a -> t a -> t a  -- infixr 1
  (%^) :: t a -> t (t a) -> t (t a)

instance (Eq a) => InsertLeft [] a where
  (%@) = (:)
  (%^) = (:)

-- | Inspired by: https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#words
-- and: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Is similar to the 'Prelude.words' but operates on more general
-- structures an allows more control.
subG :: (InsertLeft t a, Monoid (t a), Monoid (t (t a))) => t a -> t a -> t (t a)
subG whspss xs = if F.null ts then mempty else w %^ subG whspss s''
     where ts = dropWhile (`F.elem` whspss) xs
           (w, s'') = span (`F.notElem` whspss) ts

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
dropWhile' :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> (t a, t a)
dropWhile' p = F.foldr f v
  where f x (ys, xs) = (if p x then ys else x %@ xs, x %@ xs)
        v = (mempty,mempty)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
dropWhile :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> t a
dropWhile p = fst . dropWhile' p

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
span :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> (t a, t a)
span p = fst . span' p

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
span' :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> ((t a, t a), t a)
span' p = F.foldr f v
  where f x ((ys, zs), xs) = (if p x then (x %@ ys, zs) else (mempty,x %@ xs), x %@ xs)
        v = ((mempty, mempty), mempty)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
takeWhile :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> t a
takeWhile p = fst . takeWhile' p

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
takeWhile' :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> (t a, t a)
takeWhile' p = F.foldr f v
  where f x (ys,xs) = (if p x then x %@ ys else mempty, x %@ xs)
        v = (mempty,mempty)

-- | Prepends and appends the given two first arguments to the third one.
preAppend :: (InsertLeft t a, Monoid (t (t a))) => t a -> t (t a) -> t (t a) -> t (t a)
preAppend ts uss tss = mconcat [ts %^ tss, uss]
{-# INLINE preAppend #-}

-------------------------------------------------------------------------------------

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Takes the first argument quantity from the right end of the structure preserving the order.
takeFromEndG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
takeFromEndG n = (\(xs,_,_) -> xs) . F.foldr f v
 where v = (mempty,0,n)
       f x (zs,k,n)
        | k < n = (x %@ zs,k + 1,n)
        | otherwise = (zs,k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Takes the specified quantity from the right end of the structure and then reverses the result.
reverseTakeFromEndG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
reverseTakeFromEndG n = (\(xs,_,_) -> xs) . F.foldr f v
 where v = (mempty,0,n)
       f x (zs,k,n)
        | k < n = (zs `mappend` (x %@ mempty),k + 1,n)
        | otherwise = (zs,k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Is analogous to the taking the specified quantity from the structure and then reversing the result. Uses strict variant of the foldl, so is
-- not suitable for large amounts of data.
reverseTakeG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
reverseTakeG n = (\(xs,_,_) -> xs) . F.foldl' f v
 where v = (mempty,0,n)
       f (zs,k,n) x
        | k < n = (x %@ zs,k + 1,n)
        | otherwise = (zs,k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Uses strict variant of the foldl, so is
-- strict and the data must be finite.
takeG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
takeG n = (\(xs,_,_) -> xs) . F.foldl' f v
 where v = (mempty,0,n)
       f (zs,k,n) x
        | k < n = (zs `mappend` (x %@ mempty),k + 1,n)
        | otherwise = (zs,k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Is analogous to the dropping the specified quantity from the structure and then reversing the result. Uses strict variant of the foldl, so is
-- strict and the data must be finite.
reverseDropG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
reverseDropG n = (\(xs,_,_) -> xs) . F.foldl' f v
 where v = (mempty,0,n)
       f (zs,k,n) x
        | k < n = (mempty,k + 1,n)
        | otherwise = (x %@ zs,k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Drops the first argument quantity from the right end of the structure and returns the result preserving the order.
dropFromEndG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
dropFromEndG n = (\(xs,_,_) -> xs) . F.foldr f v
 where v = (mempty,0,n)
       f x (zs,k,n)
        | k < n = (mempty,k + 1,n)
        | otherwise = (x %@ zs,k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Drops the specified quantity from the right end of the structure and then reverses the result.
reverseDropFromEndG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
reverseDropFromEndG n = (\(xs,_,_) -> xs) . F.foldr f v
 where v = (mempty,0,n)
       f x (zs,k,n)
        | k < n = (mempty,k + 1,n)
        | otherwise = (zs `mappend` (x %@ mempty),k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Uses strict variant of the foldl, so is
-- strict and the data must be finite.
dropG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> t a
dropG n = (\(xs,_,_) -> xs) . F.foldl' f v
 where v = (mempty,0,n)
       f (zs,k,n) x
        | k < n = (mempty,k + 1,n)
        | otherwise = (zs `mappend` (x %@ mempty),k,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Uses strict variant of the foldl, so is
-- strict and the data must be finite.
splitAtG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> (t a, t a)
splitAtG n = (\(x,y,_,_) -> (x,y)) . F.foldl' f v
 where v = (mempty,mempty,0,n)
       f (zs,ts,k,n) x
        | k < n = (zs `mappend` (x %@ mempty),mempty,k + 1,n)
        | otherwise = (zs,ts `mappend` (x %@ mempty),k + 1,n)

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Splits the structure starting from the end and preserves the order.
splitAtEndG :: (Integral b, InsertLeft t a, Monoid (t a)) => b -> t a -> (t a, t a)
splitAtEndG n = (\(x,y,_,_) -> (y,x)) . F.foldr f v
 where v = (mempty,mempty,0,n)
       f x (zs,ts,k,n)
        | k < n = (x %@ zs,mempty,k + 1,n)
        | otherwise = (zs,x %@ ts,k + 1,n)

-- | If a structure is empty, just returns 'Nothing'.
safeHeadG :: (Foldable t) => t a -> Maybe a
safeHeadG = F.find (const True)

-- | If the structure is empty, just returns itself. Uses strict variant of the foldl, so is
-- strict and the data must be finite.
safeTailG :: (InsertLeft t a, Monoid (t a)) => t a -> t a
safeTailG = dropG 1

-- | If the structure is empty, just returns itself.
safeInitG :: (InsertLeft t a, Monoid (t a)) => t a -> t a
safeInitG = dropFromEndG 1

-- | If the structure is empty, just returns 'Nothing'.
safeLastG :: (InsertLeft t a, Monoid (t a)) => t a -> Maybe a
safeLastG = F.find (const True) . takeFromEndG 1

-----------------------------------------------------------------------------

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Acts similarly to the 'map' function from Prelude.
mapG :: (InsertLeft t b, Monoid (t b)) => (a -> b) -> t a -> t b
mapG f = F.foldr (\x ys -> f x %@ ys) mempty

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Acts similarly to 'filter' function from Prelude.
filterG :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> t a
filterG p = F.foldr (\x ys -> if p x then x %@ ys else ys) mempty

-- | Inspired by: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf. Acts similarly to 'partition' function from Data.List. Practically is a
-- rewritten for more general variants function partition from https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#partition
partitionG :: (InsertLeft t a, Monoid (t a)) => (a -> Bool) -> t a -> (t a, t a)
partitionG p = F.foldr (\x (ys,zs) -> if p x then (x %@ ys,zs) else (ys,x %@ zs)) (mempty,mempty)


