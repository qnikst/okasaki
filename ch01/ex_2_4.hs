import Data.Maybe (fromMaybe)
-- Test and bench imports
import Control.Monad.Par.Class
import Control.Applicative ((<$>))
import Criterion.Main
import Data.List (foldl')
import Test.QuickCheck
import Test.QuickCheck.Gen

data UnbalancedSet a = E | T !(UnbalancedSet a) !a !(UnbalancedSet a) 
                     deriving (Show, Eq)

empty :: UnbalancedSet a
empty = E

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
        | x /= z     = a (T E x E)
        | otherwise  = t 
    go a (T l v r) z x 
        | x <= v    = go (\t -> a (T t v r)) l v x
        | otherwise = go (\t -> a (T l v t)) r z x

insert2_3_1 :: (Ord a) => UnbalancedSet a -> a -> UnbalancedSet a
insert2_3_1 t x = fromMaybe t (go t x)
  where 
    go E x = Just (T E x E)
    go (T l v r) x | x == v = Nothing
                   | x  < v = fmap (\l' -> T l' v r) (go l x)
                   | x  > v = fmap (\r' -> T l v r') (go r x)

insert2_3_2 :: (Ord a) => UnbalancedSet a -> a -> UnbalancedSet a
insert2_3_2 t x = go id t x
  where
    go a E x = a (T E x E)
    go a (T l v r) x | x == v = t
                     | x  < v = go (\t -> a $ T t v r) l x
                     | x  > v = go (\t -> a $ T l v t) r x

insert2_2 :: (Ord a) => UnbalancedSet a -> a -> UnbalancedSet a
insert2_2 E x = T E x E
insert2_2 t@(T l v r) x | x == v = t
                        | x  < v = T (insert l x) v r
                        | x  > v = T l v (insert r x)


insert_m :: (Ord a) => UnbalancedSet a -> a -> UnbalancedSet a
insert_m t v | member t v = t
             | otherwise  = insert2_2 t v

instance NFData a => NFData (UnbalancedSet a)

main = do
  xs <- head <$> sample' (vector 102400) :: IO [Int]
  defaultMain 
    [ bench "2.4"       $ nf (foldl' insert      empty) xs
    , bench "2.3 Maybe" $ nf (foldl' insert2_3_1 empty) xs
    , bench "2.3 CPS"   $ nf (foldl' insert2_3_2 empty) xs
    , bench "2.2"       $ nf (foldl' insert2_2   empty) xs
    , bench "member"    $ nf (foldl' insert_m    empty) xs
    ]
