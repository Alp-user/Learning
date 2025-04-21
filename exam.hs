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

y = (1:2:y)
taken 0 _ = []
taken n (a:b) = a:(taken (n-1) b)

--infinite factorial list
fact = 1:1:factlist (tail fact) [2..] where
    factlist (a:b) (c:d) = (a*c):(factlist b d)

--Classes etc

data Myvalues = A Char| B Bool | C Int
instance Show Myvalues where -- show function is a->Str!!!Str
    show (A x) = show x
    show (B x) = show x
    show (C x) = show x

instance Eq Myvalues where 
    val1 == val2 = case val1 of
        (A x) -> case val2 of
            (C y) -> True
            otherwise -> False
        (B x) -> True

class Array a where
    top :: (a v) -> v
    
instance Array Mylist where
    top (Cons(a, b)) = a

-- Be fluent with reading functiop type signatures. 
double x = x*2
evenn x = x`mod`2 == 0
g1 = (foldl add 0).(map double) . (filter (evenn))
-- type signature for(foldl add 0).(map double) . (filter (evenn)) [1,2,3]: ([Int]->Int)->([Int]->[Int])->([Int]->[Int])->[Int]-> Int
-- As you can see the functions are right associative and (a->b)->(c->a) output of the function to the right should match with the input of the function to the left

iden x = x -- any function like this or where we can use multiple types are actually parameter polymorphism as they apply the same on different types.
