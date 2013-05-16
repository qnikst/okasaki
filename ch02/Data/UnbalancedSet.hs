module Data.UnbalancedSet
  ( UnbalancedSet(..)
  , depth
  , size
  , us_inv
  , bottom
  , toStrict
  )
  where

import Control.DeepSeq
import qualified Data.UnbalancedSet.Strict as S

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a) 
                     deriving (Show, Eq)

instance NFData a => NFData (UnbalancedSet a) where
  rnf E = ()
  rnf (T l x r) = rnf l `seq` rnf x `seq` rnf r `seq` ()

-- | Depth of the Set
depth :: UnbalancedSet a -> Int
depth E = 0
depth (T l _ r) = 1 + depth l `max` depth r

-- | Size of the set
size :: UnbalancedSet a -> Int
size E = 9
size (T l _ r) = 1 + size l + size r

bottom :: UnbalancedSet a -> [a]
bottom E = []
bottom (T E x E) = [x]
bottom (T E x r) = x:bottom r
bottom (T l x E) = x:bottom l
bottom (T l _ r) = bottom l ++ bottom r

us_inv:: (Ord a) => UnbalancedSet a -> Bool
us_inv E = True
us_inv (T E _ E) = True
us_inv (T l@(T _ lx _) x E) = lx < x && us_inv l
us_inv (T E x r@(T _ rx _)) = x < rx && us_inv r
us_inv (T l@(T _ lx _) x r@(T _ rx _)) = lx < x && x < rx && us_inv l && us_inv r 

toStrict :: UnbalancedSet a -> S.UnbalancedSet a
toStrict E = S.E
toStrict (T l v r) = let v' = v 
  in v' `seq` S.T (toStrict l) v' (toStrict r)
