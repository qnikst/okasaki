module Data.Queue.Class
  where

import Test.Hspec
import Test.Hspec.QuickCheck

class Queue a where
    empty :: a b
    isEmpty :: a b -> Bool
    snoc :: a b -> b -> a b
    head :: a b -> b
    tail :: a b -> a b


-- | queue stores all elements in correct order
prop_all = undefined

queue_spec = describe "is queue" $ do
    prop_all "queue contains all elements" (prop_all :: [Int] -> Bool)

