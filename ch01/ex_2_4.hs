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
insert t@(T l v r) x = fromMaybe t (go t v x)
  where 
    go E z x = if x == z 
                   then Nothing
                   else Just (T E x E)
    go (T l v r) z x | x <= v    = fmap (\l' -> T l' v r) (go l v x)
                     | otherwise = fmap (\r' -> T l v r') (go r z x)
