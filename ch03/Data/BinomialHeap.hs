module Data.BinomialHeap
  where

import Data.Function

data Tree a = Node Int a [Tree a]

type Heap a = [Tree a]

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
    | x1 < x2   = Node (r+1) x1 (t2:c1)
    | otherwise = Node (r+1) x2 (t1:c2)

root :: Tree a -> a
root (Node _ r _) = r

rank :: Tree a -> Int
rank (Node r _ _) = r

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree x [] = [x]
insTree x t@(t':ts) 
    | rank x < rank t' = x:t
    | otherwise        = t':insTree x ts

insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insTree (Node 0 x []) ts

merge :: Ord a => Heap a -> Heap a -> Heap a
merge [] x = x
merge x [] = x
merge x@(x':xs) y@(y':ys) = 
    case (compare `on` rank) x' y' of
        LT -> x':merge xs y
        GT -> y':merge x  ys
        EQ -> insTree (x' `link` y') (xs `merge` ys)

removeMinTree :: (Ord a) => Heap a -> (Tree a, Heap a)
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
    let (t',ts') = removeMinTree ts
    in if root t < root t' 
           then (t, ts)
           else (t',t:ts')

findMin :: Ord a => Heap a -> a
findMin = root . fst . removeMinTree

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts = 
    let (Node _ _ fs, ts') = removeMinTree ts
    in reverse fs `merge` ts'

