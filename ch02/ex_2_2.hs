import Control.Exception (assert)

data UnbalancedSet a = E | T !(UnbalancedSet a) !a !(UnbalancedSet a) 
                     deriving (Show, Eq)

inv1 :: (Ord a) => UnbalancedSet a -> Bool
inv1 E = True
inv1 (T E x E) = True
inv1 (T l@(T _ lx _) x E) = lx < x && inv1 l
inv1 (T E x r@(T _ rx _)) = x < rx && inv1 r
inv1 (T l@(T _ lx _) x r@(T _ rx _)) = lx < x && x < rx && inv1 l && inv1 r 

empty :: UnbalancedSet a
empty = E

{- 
- default member 
member :: (Ord a) => UnbalacedSet a -> a -> Bool
member E _ = False
member (T l x r) a | x == a = True
                   | x  > a = member l a
                   | x  < a = member r a

-}
member :: (Ord a) => UnbalancedSet a -> a -> Bool
member E _ = False
member t@(T l x r) a = go t x a
  where
    go E x a = x == a
    go (T l v r) z x | x <= v = go l v x
                     | otherwise = go r z x

insert :: (Ord a) => UnbalancedSet a -> a -> UnbalancedSet a
insert E x = T E x E
insert t@(T l v r) x | x == v = t
                     | x  < v = let res =  T (insert l x) v r
                                in assert (inv1 res) res
                     | x  > v = let res = T l v (insert r x)
                                in assert (inv1 res) res

