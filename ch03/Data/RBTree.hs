module Data.RBTree where

data Color  = R | B
data Tree a =  E | T Color (Tree a) a (Tree a)

-- Invariant1: no red node has a red child
-- Invariant2: everypath from the root to sn empty node containts 
-- the same number of black nodes

member :: (Ord a) => a -> Tree a ->  Bool
member v E = False
member v (T _ l x r) = 
    case v `compare` x of 
        LT -> v `member` l
        GT -> v `member` r
        EQ -> True

insert :: (Ord a) => a -> Tree a -> Tree a
insert x t =
    let T _ a y b = go t
    in T B a y b
  where 
    go E = T R E x E
    go (T c a y b) = 
        case x `compare` y of
            LT -> balance c (go a) y b
            GT -> balance c a y (go b)
            EQ -> t
    balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
    balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
    balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
    balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
    balance c a x b = T c a x b



