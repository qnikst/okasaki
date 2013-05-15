{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Prelude hiding (lookup)
data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a) 
                     deriving (Show, Eq)


newtype UnbalancedMap a b = M { um :: UnbalancedSet (a,b) }

class Ord k => FiniteMap a k v where
  empty  :: a k v
  bind   :: k -> v -> a k v -> a k v
  lookup :: k -> a k v -> Maybe v

instance Ord k => FiniteMap UnbalancedMap k v where
  empty = M E
  bind k v (M E) = M $ T E (k,v) E
  bind k v (M (T l (k',v') r)) 
    | k < k'    = M $ T (um $ bind k v (M l)) (k',v') r
    | k > k'    = M $ T l (k',v') (um $ bind k v (M r))
    | otherwise = M $ T l (k',v) r
  lookup _ (M E) = Nothing
  lookup k (M (T l (k',v) r)) 
    | k < k'    = lookup k (M l)
    | k > k'    = lookup k (M r)
    | otherwise = Just v
