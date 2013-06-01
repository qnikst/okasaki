module Data.BatchedDeque
  where

import Data.Deque.Class

data BatchedDeque a = BatchedDeque [a] [a]

instance Deque BatchedDeque where
    empty = BatchedDeque [] []
    isEmpty (BatchedDeque f r) = null f && null r

    cons a (BatchedDeque f r) = checkf (a:f) r

    head (BatchedDeque [] r)    = Prelude.last r
    head (BatchedDeque (x:_) _) = x

    tail (BatchedDeque [] r) = checkf [] (Prelude.tail r) 
    tail (BatchedDeque (x:f) r) = checkf f r

    snoc (BatchedDeque f r) a = checkf f (a:r)

    last (BatchedDeque f []) = Prelude.last f
    last (BatchedDeque f (x:r)) = x 

    init (BatchedDeque [] r) = checkf [] (Prelude.init r)
    init (BatchedDeque (_:xs) r) = checkf xs r


checkf :: [a] -> [a]  -> BatchedDeque a
checkf [] r = 
    let (h,t) = splitHalf (reverse r)
    in BatchedDeque h t
checkf f [] = 
    let (h,t) = splitHalf (reverse f)
    in BatchedDeque h t
checkf h r  = BatchedDeque h r

splitHalf :: [a] -> ([a],[a])
splitHalf xs = splitAt (length xs `div` 2) xs
