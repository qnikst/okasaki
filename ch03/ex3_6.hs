module Data.BinomialHeap
  where

import Data.Function

data Tree a = Node a [Tree a]

type Heap a = [(Int, Tree a)]

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
    | x1 < x2   = Node x1 (t2:c1)
    | otherwise = Node x2 (t1:c2)

root :: Tree a -> a
root (Node r _) = r

insTree :: Ord a => Int -> Tree a -> Heap a -> Heap a
insTree r x []              = [(r,x)]
insTree r x h@((r2,t'):ts)
    | r < r2 = (r,x):h
    | otherwise = (r2,t'):insTree (r2+1) (x `link` t') ts

insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insTree 0 (Node x []) ts

merge :: Ord a => Heap a -> Heap a -> Heap a
merge [] x = x
merge x [] = x
merge x@((rx,x'):xs) y@((ry,y'):ys) = 
    case rx `compare` ry of
        LT -> (rx, x'): xs `merge` y
        GT -> (ry, y'): ys `merge` x
        EQ -> insTree (rx+1) (x' `link` y') (xs `merge` ys)

removeMinTree :: (Ord a) => Heap a -> ((Int,Tree a), Heap a)
removeMinTree [t] = (t,[])
removeMinTree (t:ts) =
    let (t',ts') = removeMinTree ts
    in if root (snd t) < root (snd t')
           then (t, ts)
           else (t',t:ts')

findMin :: Ord a => Heap a -> a
findMin = root . snd . fst . removeMinTree

deleteMin :: Ord a => Heap a -> Heap a
deleteMin ts = 
    let ((r, Node _ fs), ts') = removeMinTree ts
    in (zip [0..] (reverse fs)) `merge` ts'
