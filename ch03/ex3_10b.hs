import Data.RBTree


insert :: (Ord a) => a -> Tree a -> Tree a
insert x t =
    let T _ a y b = go t
    in T B a y b
  where 
    go E = T R E x E
    go (T c a y b) = 
        case x `compare` y of
            LT -> case a of
                      E -> T c (go a) y b
                      T _ _ z _ | x <= z    -> llbalance c (go a) y b
                                | otherwise -> lrbalance c (go a) y b
            GT -> case b of
                      E -> T c a y (go b)
                      T _ _ z _ | x <= z    -> rlbalance c a y (go b)
                                | otherwise -> rrbalance c a y (go b)
            EQ -> t
    lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
    lrbalance c a x b = T c a x b
    llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
    llbalance c a x b = T c a x b
    rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
    rlbalance c a x b = T c a x b
    rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
    rrbalance c a x b = T c a x b
