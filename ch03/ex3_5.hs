import Data.BinomialHeap

findMin' :: (Ord a) => Heap a -> a
findMin' [t] = root t
findMin' (t:ts) = (root t) `min` (findMin' ts)
