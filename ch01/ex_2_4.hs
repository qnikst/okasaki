import Data.Myabe (fromMaybe)

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a) 
                     deriving (Show, Eq)

empty :: UnbalancedSet a -> Bool
empty E = True
empty _ = False

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
insert t@(T l v r) x = go id t v x
  where 
    go a E z x 
        | x == z     = t
        | otherwise  = a (T E x E)
    go a (T l v r) z x 
        | x <= v    = go (\t -> a (T t v r)) v x
        | otherwise = go (\t -> a (T l v t)) v x
