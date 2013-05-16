import Data.Heap.Class
import Data.LeftishHeap

-- Tests
import Control.DeepSeq (force)
import Data.List (foldl')
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property
import Test.QuickCheck.Assertions
import Criterion.Main


insert' :: (Ord a) => a -> LeftishHeap a -> LeftishHeap a
insert' x E = singleton x
insert' x (H i y l r) = H (rank r'' + 1) t l'' r''
  where
    (t, b) | x <= y    = (x, y) 
           | otherwise = (y, x)
    (l', r') = case (l, r) of
                  (E, E) -> (E, singleton b)
                  (h, E) -> (h, singleton b)
--                  (E, h) -> (singleton b, h)
                  (h1@(H _ y1 _ _), h2@(H _ y2 _ _)) 
                      | y1 <= y2  -> (h1, insert' b h2)
                      | otherwise -> (insert' b h1, h2)
    (l'', r'') | rank l' >= rank r' = (l', r')
               | otherwise          = (r', l')

-- TESTS
main = hspec $ do
    describe "backcompatibitiy" $ do
        prop "invariant is guarateed" 
            ((\xs ->
                let go h [] = property True -- succeeded
                    go h (x:xs) = 
                        let h' = insert' x h
                        in (property $ inv_leftish h == True) .&&. go h' xs
            in go empty xs) :: [Int] -> Property)
