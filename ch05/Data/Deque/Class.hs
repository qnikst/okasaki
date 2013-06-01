module Data.Deque.Class
  where

class Deque a where
  empty :: a b
  isEmpty :: a b -> Bool
  
  cons :: b -> a b -> a b
  head :: a b -> b
  tail :: a b -> a b

  snoc :: a b -> b -> a b
  last :: a b -> b
  init :: a b -> a b
