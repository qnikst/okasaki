data Tree a = E | T (Tree a) a (Tree a)
          deriving (Eq, Show)

mkTree :: (Ord a) => a -> Int -> Tree a
mkTree x 0 = E
mkTree x l = let n = mkTree x (l-1)
             in T n x n
