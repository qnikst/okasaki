{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Heap.Class

data ExplicitMin h a = Empty | Heap a (h a) deriving (Show)

instance (Heap h) => Heap (ExplicitMin h) where
    empty = Empty

    isEmpty Empty = True
    isEmpty _ = False

    insert v Empty = Heap v (v `insert` empty)
    insert v (Heap o h) = Heap (o `min` v) (v `insert` h)

    findMin (Heap m _) = m
    deleteMin (Heap m h) = let h' = deleteMin h 
                           in Heap (findMin h') h'

    merge Empty h = h
    merge h Empty = h
    merge (Heap m1 h1) (Heap m2 h2) = Heap (m1 `min` m2) (h1 `merge` h2)
