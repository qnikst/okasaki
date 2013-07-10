module Data.BinomialHeap.Lazy 
  where

data Tree a = Node Int a [Tree a]

type Heap a = [Tree a]

empty = []
isEmpty h = null h
rank (Node n _ _) = n
root (Node _ x _) = x

link t1@(Node r x1 c1) t2@(Node _ x2 c2)
    | x1 <= x2  = Node (r+1) x1 (t2:c1)
    | otherwise = Node (r+1) x2 (t1:c2)

insTree t [] = [t]
insTree t ts@(t':ts')
    | rank t < rank t' = t:ts
    | otherwise = insTree (t `link` t') ts'

mrg [] t  = t
mrg t  [] = t
mrg ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1:mrg ts1' ts2
    | rank t1 > rank t2 = t2:mrg ts1 ts2'
    | otherwise         = insTree (link t1 t2) (mrg ts1' ts2')

insert x ts = insTree (Node 0 x []) ts
merge ts1 ts2 = mrg ts1 ts2

removeMinTree :: (Ord a) => Heap a -> (Tree a,Heap a)
removeMinTree [] = error "empty"
removeMinTree [t] = (t,[])
removeMinTree (t:ts) = 
    let (t',ts') = removeMinTree ts
    in if root t < root t' 
           then (t,ts)
           else (t',t:ts')

findMin :: (Ord a) => Heap a -> a
findMin = root . fst . removeMinTree

deleteMin t = 
    let ((Node _ x ts),ts2) = removeMinTree t
    in mrg (reverse ts) ts2
