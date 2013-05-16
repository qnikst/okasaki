{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Data.UnbalancedSet
import Data.Set.Class

-- test & bench
import Control.DeepSeq (force)
import qualified Data.UnbalancedSet.Strict as S
import Data.List (foldl')
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property
import Test.QuickCheck.Assertions
import Criterion.Main

-- In the worst case, memper performs approximatelly $2d$ comparison
-- where $d$ is the depth of the tree . Rewrite member to take no
-- more than $d+1$ comparisons by keeping track of a candidate element
-- that \emph{might} be equal to query element
newtype Ex2_2 a  = Ex2_2  { un ::   UnbalancedSet a }

instance (Ord a) => Set Ex2_2 a where
  empty  = Ex2_2 empty
  member _ (Ex2_2 E) = False
  member x (Ex2_2 t@(T _ tv _)) = go t tv
    where
      go E z          = x == z
      go (T l v r) z
          | x <= v    = go l v
          | otherwise = go r z
  insert v = Ex2_2 . insert v . un

{- Test and benchmark section -}
newtype Ex2_2S a = Ex2_2S { unS :: S.UnbalancedSet a }

instance (Ord a) => Set Ex2_2S a where
  empty  = Ex2_2S empty
  member _ (Ex2_2S S.E) = False
  member x (Ex2_2S t@(S.T _ tv _)) = go t tv
    where
      go S.E z          = x == z
      go (S.T l v r) z
          | x <= v    = go l v
          | otherwise = go r z
  insert v = Ex2_2S . insert v . unS

main :: IO ()
main = do

    {- TESTS -}
    zs <- fmap (!! 10) $ sample' (vector 1024) :: IO [Int]
    ys <- fmap (!! 10) $ sample' (vector 1024) :: IO [Int]
    let s = force $ fromList zs
    hspec $ do
        describe "member is equal to previous one" $ do
            it "all elems are found" $ map (`member` Ex2_2 s) zs `shouldBe` (replicate 1024 True)
            prop "random list" ((\xs -> map (`member` Ex2_2 s) xs ==? map (`member` s) xs {-++zs-}) :: [Int] -> Result)
 
    {- BENCHMARKS -}
    let bs = zs++ys
        ws = bottom s ++ replicate 1024 maxBound
        s' = force $! toStrict s
    defaultMain 
        [ bgroup "common-case"
            [ bench "default member"    $ nf (map (`member` s)) bs 
            , bench "new member"        $ nf (map (`member` Ex2_2 s)) bs
            , bench "strict member"     $ nf (map (`member` s')) bs 
            , bench "strict new member" $ nf (map (`member` Ex2_2S s')) bs
            ]
        , bgroup "worst-case"
            [ bench "default member"        $ nf (map (`member` s)) ws 
            , bench "new member"            $ nf (map (`member` Ex2_2 s)) ws
            , bench "strict default member" $ nf (map (`member` s')) bs 
            , bench "strict new member"     $ nf (map (`member` Ex2_2S s')) bs
            ]
        ]
  where
    fromList :: (Set s a) => [a] -> s a
    fromList = foldl' (flip insert) empty 
