data Author = Author String Int deriving (Show, Eq)
data Book = Book String Author Int Int deriving (Eq, Show)
data Library = Library [(Book, Int, Int)] deriving (Eq, Show)

dickens = Author "Charles Dickens" 1812
wilde = Author "Oscar Wilde" 1854
john = Author "John" 2000
book1 = Book "A Tale of Two Cities" dickens 1859 430
book2 = Book "David Copperfield" dickens 1850 1024
book3 = Book "The Picture of Dorian Gray" wilde 1890 304
book4 = Book "Sparkles" wilde  2020 85
book5 = Book "Pride and Prejudice" john 2005 432
book6 = Book "Jane Eyre" wilde 1847 532
lib = Library [(book5,2,2),(book1, 6, 2), (book2, 3, 3), (book3, 4, 3), (book4, 1, 0)]


---------- Do not change anything above this line ----------

publicationAge :: Book -> Int
publicationAge (Book _ (Author _ y) p _) = p - y

booksByAuthor :: Author -> Library -> [Book]
booksByAuthor (Author a _) (Library l) = [xs| (xs, _, _)<-l , (\(Book _ (Author name _) _ _) a -> (name == a)) xs a] -- You need to specify value constructor/tags to have correct pattern matching

shorter_than_201 (Book _ _ _ page) = page < 201

shortBooks :: Library -> [Book]
shortBooks (Library l) = [x |(x, _, _)<-l, shorter_than_201 x ]


bookInCollection :: String -> Library -> Bool
bookInCollection name (Library l)= inner_work l where
    inner_work [] = False
    inner_work (((Book bn _ _ _), _ ,_):b)  = if name == bn then True else inner_work b  

reduce_Sum f s [] = s
reduce_Sum f s ((Book _ _ _ a):b) = reduce_Sum f (f s a) b


totalPagesByAuthor :: Author -> Library -> Int
totalPagesByAuthor x  lst = let 
        book_list = booksByAuthor x lst
        inner_work = reduce_Sum (\x y ->x+y) 0 
    in inner_work book_list 

find_sum [] r = r 
find_sum (((Book _ _ _ pages), _, _):b) (sumit, numit) = find_sum b (sumit+pages, numit + 1)
averageBookLength :: Library -> Int
averageBookLength (Library li) = let 
        (x,y) = find_sum li (0,0)
    in (x `div` y)
inList x [] = False
inList x (a:b) = if x == a then True else inList x b
elimDups [] = []
elimDups (a:b) = if inList a b then elimDups b else a:(elimDups b ) 

authorsWithLongBooks :: Int -> Library -> [Author]
authorsWithLongBooks min_page (Library lst) = elimDups [aut | ((Book _ aut _ pages), _ , _)<-lst, pages >= min_page]

choose_min (n1 , x, bir1) (n2, y, bir2) = if x < y then (n1,x, bir1) else (n2,y, bir2) 

age_list li = [(name, publ - bir, bir)| ((Book _ (Author name bir) publ _), _, _)<-li]


findYoungestAuthor :: Library -> Author
findYoungestAuthor (Library li) = let (nf, _, bf) = inner_work choose_min ("some", 1000000, 0) (age_list li) in
    Author nf bf
    where
        inner_work f s [] = s
        inner_work f s (a:b) = inner_work f (f a s) b


iterative_bool name [] = True
iterative_bool name (((Book name2 _ _ _), _ , _):b) = if name == name2 then False else iterative_bool name b 

iterative_bool2 name [] = (False, 0)
iterative_bool2 name ((x,(Book name2 _ _ _)):b) = if name == name2 then (True,x) else iterative_bool2 name b

nested_difference lst1 lst2 = [((Book name aut x y), num, num) | (num ,(Book name aut x y))<-lst1, iterative_bool name lst2] 
nested_difference2 lst1 lst2 = [if booli then ((Book name aut x y), n1 + num, n2 + num ) else ((Book name aut x y), n1, n2) |((Book name aut x y),n1,n2)<-lst1 ,let (booli, num)= iterative_bool2 name lst2]

myappend [] lst2 = lst2
myappend (a:b) lst2 = a:(myappend b lst2)

newArrivals :: Library -> [(Int, Book)] -> Library
newArrivals (Library li) lst = Library (myappend (nested_difference2 li lst) (nested_difference lst li))

-- This is a helper function that gives you a list of just the books in a library,
-- with the [Book] type, without the library availability information.
-- It might come in handy for many questions.
-- You may change and improve it to better fit you solution, if you want to.
getBooks :: Library -> [Book]
getBooks (Library books) = map (\(x,_,_) -> x) books

