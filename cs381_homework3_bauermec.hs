{--
    Homework 3 - Caleb Bauermeister
--}

------------------------------------
-- Exercise 1
------------------------------------

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int
         
type Rank    = Int
type CmdRank = (Int, Int)

----------------------------
-- (a)

rankC :: Cmd -> CmdRank
rankC ADD     = (2, 1)
rankC MULT    = (2, 1)
rankC DUP     = (1, 2)
rankC INC     = (1, 1)
rankC SWAP    = (2, 2)
rankC (LD _)  = (0, 1)
rankC (POP i) = (i, 0)

rank :: Prog -> Maybe Rank
rank [] = Just 0
rank xs = rankS xs 0


rankS :: Prog -> Rank -> Maybe Rank
rankS [] r = Just r
rankS (x:xs) i 
    | i >= po   = rankS xs (i - po + pu)
    | otherwise = Nothing  
    where (po, pu) = (rankC x) 
    
----------------------------
-- (b)

type D = (Stack -> Stack)
type Stack = [Int]

sem :: Prog -> D
sem [] s = s
sem (c:cs) s = sem cs ( semCmd c s )


-- Error checking removed because of new checker function

semCmd :: Cmd -> D
semCmd (LD x) (xs)     = (x:xs)
semCmd (DUP) (x:xs)    = (x:x:xs)
semCmd (ADD) (x:y:xs)  = ((x + y):xs)
semCmd (MULT) (x:y:xs) = ((x * y):xs)
semCmd (INC) (x:xs)    = ((x + 1):xs)
semCmd (SWAP) (x:y:xs) = (y:x:xs)
semCmd (POP 0) (xs)    = (xs)
semCmd (POP i) (x:xs)  = semCmd (POP (i - 1)) xs


rankSafe :: Prog -> Bool
rankSafe xs = rank xs /= Nothing

semStatTC :: Prog -> Stack -> Maybe Stack
semStatTC xs x | rankSafe xs = Just (sem xs x)
               | otherwise   = Nothing
             
{-- 
    In my previous homework the function sem had the type of
    Maybe Stack -> Maybe Stack. Because the handling of errors
    Now takes place in another part of the program the sem function
    can be simplified to remove a lot of "Justs"

--}             
                       
----------------------------
-- Tests

-- Success
exer1_c1 = semStatTC [LD 3, LD 5, ADD] []
exer1_c2 = semStatTC [LD 3, LD 5, MULT] []
exer1_c3 = semStatTC [LD 3, DUP] []
exer1_c4 = semStatTC [LD 3, INC] []
exer1_c5 = semStatTC [LD 3, LD 5, SWAP] []
exer1_c6 = semStatTC [LD 3, DUP, DUP, DUP, POP 2 ] []
exer1_c7 = semStatTC [] []

--Errors (returns Nothing)
exer1_e1 = semStatTC [LD 3, ADD] []
exer1_e2 = semStatTC [LD 3, MULT] []
exer1_e3 = semStatTC [DUP] []
exer1_e4 = semStatTC [INC] []
exer1_e5 = semStatTC [LD 3, SWAP] []
exer1_e6 = semStatTC [LD 3, POP 8] []


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

------------------------------------
-- Exercise 3
------------------------------------

------------------------------------
-- (a)

f x y = if null x then [y] else x
g x y = if not (null x) then [] else [y]


{--

    (1) f :: [a] -> a -> [a]
        g :: [a] -> b -> [b]
        
    (2) For f we know that x must a least be a list because of use of null x.
        Then since we return a list of [y] in one case we know that y most be the same as
        the type that makes up the list of x
        
        For g we again know x must be a list. Since we return either a list of y or en empty list
        x and y need not be related like in function f and it can be more general
        
    (3) G is more general since it will accept different types for x and y
    
    (4) As I stated above function g returns either an empty list or a list of y. 
        This is different than function f which has the possibility of returning
        x, meaning that x and y have to be related. In Function g they do not have ot be.

--}

------------------------------------
-- (b)

-- h :: [b] -> [(a, b)] -> [b]
h x y = if null x then map snd y else x  

------------------------------------
-- (c)

-- k :: (a -> b) -> ((a -> b) -> a) -> b
k f g = f $ g f

------------------------------------
-- (d)

{--

    It is hard to change the type if we know nothing about it. In all the earlier examples we were
    given enough arguments that we could change around generic types into new structures. To get
    a -> b to work we would need to know more about a. But since we don't this function is very hard
    to construct.

--}
