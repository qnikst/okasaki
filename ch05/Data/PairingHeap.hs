module Data.PairingHeap
  where


data Heap a = E | T a [Heap a]


empty = E
isEmpty E = True
isEmpty _ = False


merge h E = h
merge E h = h
merge h1@(T x hs1) h2@(T y hs2)
  | x <= y    = T x $ h2:hs1
  | otherwise = T y $ h1:hs2

insert x = merge (T x [])

mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = (merge h1 h2) `merge` (mergePairs hs)

findMin E = error "empty"
findMin (T x _) = x

deleteMin (T _ hs) = mergePairs hs
deleteMin _ = error "empty"
