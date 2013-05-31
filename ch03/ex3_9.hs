import Data.RBTree
import Data.List 
import Debug.Trace

fromOrdList :: [a]  -> Tree a
fromOrdList [] = E
fromOrdList xs =
    let T _ x y z = go 0 maxdepth xs
    in  T B x y z
  where
    go _ _ []      = E
    go d md [x]     = T (color (d+1) md) E x E
    go d md [x,y]   = T B (T (color (d+2) md) E x E) y E
    go d md [x,y,z] = T B (T (color (d+2) md) E x E) y (T (color (d+2) md) E z E)
    go d md ys      = T B (go (d+1) md l) t (go (d+1) md r)
      where
        (l,t:r) = splitAt (length ys `div` 2) ys -- N+N
    color d md = if d == md 
                    then R
                    else B
    maxdepth :: Int                   
    maxdepth = go (length xs) 1 2
      where
        go l z y | l < y = z
                 | otherwise = go l (z+1) (y*2)

-- test
