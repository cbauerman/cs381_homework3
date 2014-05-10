------------------------------------
-- Homework 3 - Caleb Bauermeister
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
rankC DUP     = (0, 1)
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
run1_c1 = semStatTC [LD 3, LD 5, ADD]

--Errors
run1_e1 = semStatTC [LD 3, ADD]

