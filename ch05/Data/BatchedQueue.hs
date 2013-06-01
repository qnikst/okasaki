module Data.BatchedQueue
  where

import Data.Queue.Class

data BatchedQueue a = BatchedQueue [a] [a]

instance Queue BatchedQueue where
    empty = BatchedQueue [] []
    isEmpty (BatchedQueue f r) = null r

    snoc (BatchedQueue f r) a = checkf f (a:r)
    head (BatchedQueue [] _)    = error "empty"
    head (BatchedQueue (x:_) _) = x

    tail (BatchedQueue [] _)    = error "emtpy"
    tail (BatchedQueue (x:f) r) = checkf f r

checkf :: [a] -> [a]  -> BatchedQueue a
checkf [] r = BatchedQueue (reverse r) []
checkf h r  = BatchedQueue h r
