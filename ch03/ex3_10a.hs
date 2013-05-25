import Data.RBTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x t =
    let T _ a y b = go t
    in T B a y b
  where 
    go E = T R E x E
    go (T c a y b) = 
        case x `compare` y of
            LT -> lbalance c (go a) y b
            GT -> rbalance c a y (go b)
            EQ -> t
    lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
    lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
    lbalance c a x b = T c a x b
    rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
    rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
    rbalance c a x b = T c a x b
