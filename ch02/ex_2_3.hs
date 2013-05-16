{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Data.UnbalancedSet
import Data.Set.Class

import Data.Maybe (fromMaybe)

-- test & bench
import Control.DeepSeq
import qualified Data.UnbalancedSet.Strict as S
import Data.List (foldl')
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property
import Test.QuickCheck.Assertions
import Criterion.Main
import Data.List (foldl', nub)

newtype Ex2_3_1 a = Ex2_3_1 { un :: UnbalancedSet a}

instance (Ord a) => Set Ex2_3_1 a where
  empty = Ex2_3_1 empty
  member x = member x . un 
  -- using maybe
  insert x (Ex2_3_1 t) = Ex2_3_1 $! fromMaybe t (go t x)
    where 
      go E x = Just (T E x E)
      go (T l v r) x 
          | x == v = Nothing
          | x  < v = fmap (\l' -> T l' v r) (go l x)
          | x  > v = fmap (\r' -> T l v r') (go r x)

newtype Ex2_3_2 a = Ex2_3_2 { un2 :: UnbalancedSet a}

instance (Ord a) => Set Ex2_3_2 a where
  empty = Ex2_3_2 empty
  member x = member x . un2 
  -- using CPS
  insert x (Ex2_3_2 t) = Ex2_3_2 $! go id t x
    where
      go a E x = a (T E x E)
      go a (T l v r) x 
         | x == v = t
         | x  < v = go (\t -> a $ T t v r) l x
         | x  > v = go (\t -> a $ T l v t) r x

--  Test and benchmark section
--
instance NFData a => NFData (Ex2_3_1 a) where
  rnf (Ex2_3_1 c) = rnf c `seq` ()

instance NFData a => NFData (Ex2_3_2 a) where
  rnf (Ex2_3_2 c) = rnf c `seq` ()

main = do

    {- TESTS -}
--    ys <- fmap (!! 10) $ sample' (vector 1024) :: IO [Int]
--    let s = force $ fromList zs
    hspec $ do
        describe "member is equal to previous one" $ do
            prop "maybe equiv to original" 
                ((\xs -> fromList xs ?== un  (fromList xs)) :: [Int] -> Result)
            prop "CPS equiv to original" 
                ((\xs -> fromList xs ?== un2 (fromList xs)) :: [Int] -> Result)

    {- BENCHMARKS -}
    bs <- fmap (!! 10) $ sample' (vector 16768) :: IO [Int]
    let bs1 = combine bs (replicate 16768 666)
    defaultMain 
        [ bgroup "common-case"
            [ bench "default"    $ nf (fromList :: [Int] -> UnbalancedSet Int) bs 
            , bench "maybe"      $ nf (fromList :: [Int] -> Ex2_3_1 Int) bs
            , bench "CPS"        $ nf (fromList :: [Int] -> Ex2_3_2 Int) bs
            ]
        , bgroup "many eq"
            [ bench "default"    $ nf (fromList :: [Int] -> UnbalancedSet Int) bs1
            , bench "maybe"      $ nf (fromList :: [Int] -> Ex2_3_1 Int) bs1
            , bench "CPS"        $ nf (fromList :: [Int] -> Ex2_3_2 Int) bs1
            ]
        , bgroup "simple-case"
            [ bench "default"    $ nf (fromList :: [Int] -> UnbalancedSet Int) [1..1024]
            , bench "maybe"      $ nf (fromList :: [Int] -> Ex2_3_1 Int) [1..1024]
            , bench "CPS"        $ nf (fromList :: [Int] -> Ex2_3_2 Int) [1..1024]
            ]
        ]
  where
    fromList :: (Set s a) => [a] -> s a
    fromList = foldl' (flip insert) empty 
    {-# INLINE fromList #-}
    combine :: [a] -> [a] -> [a]
    combine [] ys = ys
    combine (x:xs) ys = x:combine ys xs
--    quickCheck ((\xs -> fromList xs == fromList (nub xs)) :: [Int] -> Bool)
--    quickCheck ((\xs -> all (member (fromList xs)) xs) :: [Int] -> Bool) 
--    quickCheck ((\xs ys -> all (\x -> elem x xs == member (fromList xs) x) ys):: [Int]->[Int]->Bool)
