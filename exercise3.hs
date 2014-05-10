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
        
    (2)
    (3) G is more general since it will accept different types for x and y
    (4) 

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

    It is hard to change the type if we know nothing about it.

--}