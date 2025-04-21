add :: Integer -> Integer -> Integer
add x y = x+y -- nested mapping
cadd (x,y) = x+y -- cartesian mapping
a = 34546456

data Mylist a = Lempty | Cons (a, Mylist a) deriving Show-- List = Int*List + NULL (1,(2,(3,Empty))) -- example

llen Lempty = 0
llen (Cons(a, b)) = 1+(llen b)
--Haskell does not have actual array

--Nameless functions are aggregates!!! As they are composite expressions that do not require to be declared!!!
f = \x -> \y-> x+y

x +++ y = x+y-y-x-- you can define your own operators

fib 1 s _ = s
fib n s o = fib (n-1) (s+o) s
callfib n = fib n 1 1

Cons(a , _) !!! 0 = a
Cons(a, Lempty) !!! _ = a
Cons(a, b) !!! n = b !!! (n-1) 
x = Cons(0, Cons(2, Cons(4, Cons(6, Cons(8,Lempty)))))

powerset [] = [[]]
powerset (a:b) = let smallerset = powerset b in
    smallerset ++ [a:x| x<-smallerset]
                     

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

insertit Empty elem = (Node elem Empty Empty) 

insertit n@(Node x t1 t2) elem 
    | elem > x = (Node x t1 (insertit t2 elem))
    | elem < x = (Node x (insertit t1 elem) t2)
    | elem == x = n

gls = [(x,y)| x<-[1..5], y<-[2,4,6]]

iter f s 0 = s
iter f s n = f (iter f s (n-1))
