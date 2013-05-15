import Data.Maybe (fromMaybe)

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a) 
                     deriving (Show, Eq)

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
insert t x = fromMaybe t (go t x)
  where 
    go E x = Just (T E x E)
    go (T l v r) x | x == v = Nothing
                   | x  < v = fmap (\l' -> T l' v r) (go l x)
                   | x  > v = fmap (\r' -> T l v r') (go r x)

insert1 :: (Ord a) => UnbalancedSet a -> a -> UnbalancedSet a
insert1 t x = go id t x
  where
    go a E x = a (T E x E)
    go a (T l v r) x | x == v = t
                     | x  < v = go (\t -> a $ T t v r) l x
                     | x  > v = go (\t -> a $ T l v t) r x
