data Tree a = E | T (Tree a) a (Tree a)
          deriving (Eq, Show)

mkTree :: (Ord a) => a -> Int -> Tree a
mkTree x 0 = E
mkTree x s = let sl = (s - 1) `div` 2
                 sr = (s - 1 - sl)
                 ln = mkTree x sl
                 rn = if sl == sr
                          then ln
                          else mkTree x sr
             in T ln x rn
