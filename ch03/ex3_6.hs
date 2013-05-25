module Data.BinomialHeap
  where

import Data.Heap.Class
import Data.Function
import Test.Hspec
import Debug.Trace

data Tree a = Node a [Tree a] deriving (Show)

type THeap a = [(Int, Tree a)]

newtype BHeap a = BHeap { unBHeap :: THeap a} deriving (Show)

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2)
    | x1 < x2   = Node x1 (t2:c1)
    | otherwise = Node x2 (t1:c2)

root :: Tree a -> a
root (Node r _) = r

instance Heap BHeap where
    empty = BHeap []
    isEmpty (BHeap []) = True
    isEmpty _          = False
    insert x (BHeap ts) = BHeap $ insTree 0 (Node x []) ts
    merge (BHeap []) x = x
    merge x (BHeap []) = x
    merge (BHeap x@((rx,x'):xs)) (BHeap y@((ry,y'):ys)) = BHeap $
        case rx `compare` ry of
            LT -> (rx, x'): unBHeap ((BHeap xs) `merge` (BHeap y))
            GT -> (ry, y'): unBHeap ((BHeap x) `merge` (BHeap ys))
            EQ -> insTree (rx+1) (x' `link` y') (unBHeap $ (BHeap xs) `merge` (BHeap ys))
    findMin = root . snd . fst . removeMinTree . unBHeap
    deleteMin ts = (BHeap (reverse $ zip [r-1,r-2..0] fs)) `merge` (BHeap ts')
      where
        ((r, Node _ fs), ts') = removeMinTree (unBHeap ts)


insTree :: Ord a => Int -> Tree a -> THeap a -> THeap a
insTree r x []              = [(r,x)]
insTree r x h@((r2,t'):ts)
    | r < r2 = (r,x):h
    | otherwise = insTree (r2+1) (x `link` t') ts


removeMinTree :: (Ord a) => THeap a -> ((Int,Tree a), THeap a)
removeMinTree [t] = (t,[])
removeMinTree (t:ts) =
    let (t',ts') = removeMinTree ts
    in if root (snd t) <= root (snd t')
           then (t, ts)
           else (t',t:ts')

binomial_heap_tests = do
    heap_spec (T :: T (BHeap Int))
--    leftish_heap_spec
