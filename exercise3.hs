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