import Data.PairingHeap

data BinTree a = BE | BT (BinTree a) a (BinTree a)
               deriving (Eq, Show)

toBinary :: (Ord a) => [Heap a] -> BinTree a
toBinary [] = BE
toBinary (E:xs) = BE
toBinary ((T x hs):xs) = BT  (toBinary hs) x (toBinary xs)



bempty = BE

bisEmpty BE = True
bisEmpty _ = False

bmerge h BE = h
bmerge BE h = h
bmerge h1@(BT ch1 x sb1) h2@(BT ch2 y sb2)
  | x <= y    = BT ch1 x (h2 `bmerge` sb1)
  | otherwise = BT ch2 y (h1 `bmerge` sb2)

binsert x = bmerge (BT BE x BE)

bmergePairs BE = BE
bmergePairs (BT c x BE) = BT c x BE
bmergePairs (BT c x s@(BT{})) = (BT c x BE) `bmerge` bmergePairs s

bfindMin BE = error "empty"
bfindMin (BT _ x _) = x

bdeleteMin (BT _ x hs) = bmergePairs hs
bdeleteMin _ = error "empty"
