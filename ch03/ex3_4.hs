-- see Data/WeightBasedHeap.hs
import Data.Heap.Class
import Data.WeightBasedHeap as W
import Data.LeftishHeap as L
import Test.QuickCheck

toWB :: LeftishHeap a -> WeightBasedHeap a
toWB L.E = W.E
toWB (L.H i x a b) = W.H i x (toWB a) (toWB b)

toLH :: WeightBasedHeap a -> LeftishHeap a
toLH W.E = L.E
toLH (W.H i x a b) = L.H i x (toLH a) (toLH b)

test = do
  quickCheck (L.inv_leftish . fromList :: [Int] -> Bool)
  quickCheck (W.inv_weight . fromList :: [Int] -> Bool)
  quickCheck (L.inv_leftish . toLH . fromList :: [Int] -> Bool)
  quickCheck (W.inv_weight . toWB . fromList :: [Int] -> Bool)
