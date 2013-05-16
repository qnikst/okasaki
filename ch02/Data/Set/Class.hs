{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Set.Class
  ( Set(..)
  ) where

import Data.UnbalancedSet
import qualified Data.UnbalancedSet.Strict as S

class Set a b where
  empty  :: a b
  insert :: b -> a b -> a b
  member :: b -> a b -> Bool

instance (Ord a) => Set UnbalancedSet a where
  empty = E

  member _ E = False
  member x (T l v r)
    | x == v = True
    | x  > v = member x r
    | x  < v = member x l

  insert x E = T E x E
  insert x t@(T l v r)
    | x == v = t
    | x  < v = T (insert x l) v r
    | x  > v = T l v (insert x r)

instance (Ord a) => Set S.UnbalancedSet a where
  empty = S.E

  member _ S.E = False
  member x (S.T l v r)
    | x == v = True
    | x  > v = member x r
    | x  < v = member x l

  insert x S.E = S.T S.E x S.E
  insert x t@(S.T l v r)
    | x == v = t
    | x  < v = S.T (insert x l) v r
    | x  > v = S.T l v (insert x r)
