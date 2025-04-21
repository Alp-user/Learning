even_square lst = [x^2 | x<-lst , x `mod` 2 == 0]

sum_seven = [(x,y)| x<-[1,2,3,4,5], y<-[1,2,3,4,5], x+y == 7]

match_vowel y = checked_for_you y ['a', 'e', 'i', 'o', 'u'] where
    checked_for_you x [] = False
    checked_for_you x (a:b) = if x == a then True else checked_for_you x b

remove_vowel str = [x | x<- str, not (match_vowel x)]

my_append [] lst2 = lst2
my_append (a:b) lst2 = a:(my_append b lst2)

nice_reverse  lst = ugly_reverse lst [] where
    ugly_reverse [] lst2 = lst2
    ugly_reverse (a:b) lst2 = ugly_reverse b (a:(lst2)) 

i_am_flattened lst = [x | bigx <- lst, x<-bigx]

double_me x = x*2
halve_me x = x/2

map_me f [] = []
map_me f (a:b) = (f a):(map_me f b)

string_count [] = 0
string_count (a:b) = 1 + string_count (b)

count_all f = let
    inner_count lst = [f x| x<-lst]
    in inner_count

add_five x = x + 5

map_five f lst = [f x| x<-lst]

higherone = map_five add_five

positive x = x>0

positive_filter f lst = [x|x<-lst, positive x] 

higher_second = positive_filter positive

shorter_than_n  n word = inner_count 0 word
    where
        inner_count length []
            | length > n = False
            | otherwise = True 
        inner_count length (a:b)
            | length > n  = False
            | otherwise = inner_count (length+1) b

filter_length f n [] = []
filter_length f n (a:b) = if (f n a) then a:(filter_length f n b) else filter_length f n b 


add x y = x + y
list_sum f s [] = s
list_sum f s (a:b) = list_sum f (f s a) b

myproduct x y = x*y
list_product_righttoleft f s [] = s
list_product_righttoleft f s (a:b) = f a (list_product_righttoleft f s b)

apply_n_times f n s
    | n == 0 = s
    | otherwise = apply_n_times f (n-1) (f s)
