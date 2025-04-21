data MyList a = Cons (Int, MyList Int) | Empty deriving Show

x :: MyList Int
x = Cons(4, Cons(3, Cons(2,Empty)))

sf Empty = 0
sf (Cons(a, b)) = a^2 + sf b

alternate_list ([]) x = []
alternate_list (a:b) x = if x == True then a:(alternate_list b False) else (alternate_list b True) -- (a:b x) is interpreted as b x function and a as a list due to the precedence of the functions in haskell

-- ++ is the append operator for lists.
-- Record Syntax

data Person = Person String String String String Int deriving Show
first_name  (Person x _ _ _ _) = x
sur_name (Person _ x _ _ _) = x

--instead of doing this, there is:

-- Function and variable names in Haskell must start with lower case letters.
data BetterPerson = BetterPerson {firstname :: String, surname :: String, nickname :: String, othername :: String, numbername :: Int} deriving Show
xx = BetterPerson{firstname = "alp", surname = "Arargüç", nickname = "a", othername = "b", numbername = 5} 
--or 
y = BetterPerson "Alp" "Arar" "a" "b" 4 -- you do not say BetterPerson y. This is not cpp. There is no such explicit declaration here except for ::.

absvalue num = if (num > 0) then num else (num * (-1))

signifier ohno
    | ohno > 0 = "Positive"
    | ohno == 0 = "hmmm" 
    | otherwise = "Negative"


