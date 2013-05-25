
import Data.LeftishHeap 
import Data.Heap.Class

fromList' :: Ord a => [a] -> LeftishHeap a
fromList' = head . go . (map singleton)
  where
    go []       = [empty]
    go [x1]     = [x1]
    go [x1,x2]  = [x1 `merge` x2]
    go (x1:x2:xs) = go $ (x1 `merge` x2) : go xs
