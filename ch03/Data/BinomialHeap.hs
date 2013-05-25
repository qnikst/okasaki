module Data.BinomialHeap
  where

import Data.Heap.Class
import Data.Function
import Test.Hspec

data Tree a = Node Int a [Tree a]

newtype BHeap a = BHeap { unBHeap :: [Tree a]}

link :: (Ord a) => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
    | x1 < x2   = Node (r+1) x1 (t2:c1)
    | otherwise = Node (r+1) x2 (t1:c2)

root :: Tree a -> a
root (Node _ r _) = r

rank :: Tree a -> Int
rank (Node r _ _) = r

instance Heap BHeap where
    empty = BHeap []
    isEmpty (BHeap []) = True
    isEmpty _  = False
    insert x ts = insTree (Node 0 x []) ts
    merge (BHeap []) x = x
    merge x (BHeap []) = x
    merge (BHeap x@(x':xs)) (BHeap y@(y':ys)) = 
        case (compare `on` rank) x' y' of
            LT -> BHeap $ x':(unBHeap $ merge (BHeap xs) (BHeap y))
            GT -> BHeap $ y':(unBHeap $ merge (BHeap x) (BHeap ys))
            EQ -> insTree (x' `link` y') ((BHeap xs) `merge` (BHeap ys))
    findMin = root . fst . removeMinTree
    deleteMin ts = 
        let (Node _ _ fs, ts') = removeMinTree ts
        in (BHeap (reverse fs)) `merge` ts'

insTree :: Ord a => Tree a -> BHeap a -> BHeap a
insTree x (BHeap []) = BHeap [x]
insTree x (BHeap t@(t':ts)) 
    | rank x < rank t' = BHeap (x:t)
    | otherwise        = insTree (x `link` t') (BHeap ts)


removeMinTree :: (Ord a) => BHeap a -> (Tree a, BHeap a)
removeMinTree (BHeap [t]) = (t, BHeap [])
removeMinTree (BHeap (t:ts)) =
    let (t',ts') = removeMinTree (BHeap ts)
    in if root t < root t' 
           then (t, BHeap ts)
           else (t',BHeap  (t:unBHeap ts'))

binomial_heap_tests = do
    heap_spec (T :: T (BHeap Int))
--    leftish_heap_spec
