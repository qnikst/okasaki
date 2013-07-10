import qualified Data.BinomialHeap.Lazy as HL

data SizedHeap a = SizedHeap Int (HL.Heap a)

empty = SizedHeap 0 HL.empty

insert a (SizedHeap x h) = SizedHeap (x+1) (HL.insert a h)

findMin (SizedHeap _ h) = HL.findMin h

deleteMin (SizedHeap x h) = SizedHeap (x-1) (HL.deleteMin h)

