------------------------------------
-- Exercise 2
------------------------------------

data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show
           
type BBox = (Int, Int)

------------------------------------
-- (a)

bbox :: Shape -> BBox
bbox X = (1, 1)
bbox (LR s1 s2) = ((s1x + s2x), (max s1y s2y) ) 
    where (s1x, s1y) = bbox s1
          (s2x, s2y) = bbox s2
bbox (TD s1 s2) = ((max s1x s2x), (s1y + s2y)) 
    where (s1x, s1y) = bbox s1
          (s2x, s2y) = bbox s2
          
------------------------------------
-- (b)

rect :: Shape -> Maybe BBox
rect X = Just (1, 1)
rect (LR s1 s2) 
    | s1y == s2y = Just ((s1x + s2x), (max s1y s2y) )
    | otherwise  = Nothing
    where Just (s1x, s1y) = rect s1
          Just (s2x, s2y) = rect s2
rect (TD s1 s2) 
    | s1x == s2x = Just ((max s1x s2x), (s1y + s2y)) 
    | otherwise  = Nothing
    where Just (s1x, s1y) = rect s1
          Just (s2x, s2y) = rect s2
          
----------------------------
-- Tests

-- Success
exer2_c1 = bbox $ LR (TD X X) X
exer2_c2 = rect $ LR (TD X X) (TD X X)
exer2_c3 = rect $ TD (LR X X) (LR X X)

-- Errors (Return Nothing)
exer2_e1 = rect $ LR (TD X X) X
exer2_e2 = rect $ TD (LR X X) X
