import Data.Heap.Class
import Data.LeftishHeap

-- Tests
import Control.DeepSeq (force)
import Data.List (foldl')
import Test.Hspec
import Test.Hspec.QuickCheck

insert' :: (Ord a) => a -> LeftishHeap a -> LeftishHeap a
insert' x E = singleton x
insert' x (H i y l r) = H (rank r'' + 1) t l'' r''
  where
    (t, b) | x <= y    = (x, y) 
           | otherwise = (y, x)
    (l', r') = case (l, r) of
                  (E, E) -> (E, singleton b)
                  (h, E) -> (h, singleton b)
                  (h1@(H _ y1 _ _), h2@(H _ y2 _ _)) 
                      | y1 <= y2  -> (h1, insert' b h2)
                      | otherwise -> (insert' b h1, h2)
    (l'', r'') | rank l' >= rank r' = (l', r')
               | otherwise          = (r', l')

-- Class instance
newtype Ex3_2 a = Ex3_2 { unEx3_2 :: LeftishHeap a}

instance Heap Ex3_2 where
    empty   = Ex3_2 empty
    isEmpty (Ex3_2 w)   = isEmpty w
    insert  x (Ex3_2 w) = Ex3_2 $ insert' x w
    merge   (Ex3_2 w1) (Ex3_2 w2) = Ex3_2 $ merge w1 w2
    findMin (Ex3_2 w)   = findMin w 
    deleteMin (Ex3_2 w) = Ex3_2 $ deleteMin w
    
-- Specs and tests
ex3_2_spec = do
    describe "ex3 is correct leftish heap" $
        prop "is balanced" (prop_balanced :: [Int] -> Bool)
  where
    prop_balanced = inv_leftish . unEx3_2 . fromList

ex3_2_tests = do
    heap_spec (T :: T (Ex3_2 Int))
    ex3_2_spec
    -- TODO: reuse leftish heap spec
    --    leftish_heap_spec (T :: T (Ex3_2 Int))
