data Author = Author String Int deriving (Eq, Show)
data Book = Book String Author Int Int deriving (Eq, Show)
data Library = Library [(Book, Int, Int)] deriving (Eq, Show)

publicationAge :: Book -> Int
publicationAge (Book _ (Author _ birth) year _) = year - birth

booksByAuthor :: Author -> Library -> [Book]
booksByAuthor author = (filter sameAuthor) . getBooks
    where sameAuthor (Book _ bookAuthor _ _) = author == bookAuthor

shortBooks :: Library -> [Book]
shortBooks = (filter bookIsShort) . getBooks
    where bookIsShort (Book _ _ _ pages) = pages <= 200

bookInCollection :: String -> Library -> Bool
bookInCollection bookName = (elem bookName) . (map getBookName) . getBooks
    where getBookName (Book name _ _ _) = name

totalPagesByAuthor :: Author -> Library -> Int
totalPagesByAuthor author = sum . (map getBookPages) . (booksByAuthor author)
    where getBookPages (Book _ _ _ pages) = pages

averageBookLength :: Library -> Int
averageBookLength lib = (sum . (map getBookPages)) books `div` length books
    where getBookPages (Book _ _ _ pages) = pages
          books = getBooks lib

authorsWithLongBooks :: Int -> Library -> [Author]
authorsWithLongBooks len = (foldr addUnique []) . (map getBookAuthor) . (filter bookLongEnough) . getBooks
    where getBookAuthor (Book _ author _ _) = author
          bookLongEnough (Book _ _ _ pages) = pages >= len

findYoungestAuthor :: Library -> Author
findYoungestAuthor lib = snd . (foldr getOlderAuthor firstAuthor) . (map ageAndAuthor) . getBooks $ lib
    where getOlderAuthor (age1, author1) (age2, author2)
              | age1 < age2 = (age1, author1)
              | otherwise   = (age2, author2)
          ageAndAuthor book@(Book _ author _ _) = (publicationAge book, author)
          firstAuthor = (ageAndAuthor . (!!0) . getBooks) lib

newArrivals :: Library -> [(Int, Book)] -> Library
newArrivals (Library records) = Library . (foldl addBook records)
    where addBook [] (count, book) = [(book, count, count)]
          addBook ((b,n,a):bs) (count, book)
              | b == book = (book, n+count, a+count):bs
              | otherwise = (b,n,a):(addBook bs (count, book))

-- These two functions are helpers
getBooks :: Library -> [Book]
getBooks (Library books) = map (\(x,_,_) -> x) books

addUnique :: Eq a => a -> [a] -> [a]
addUnique el lst
    | el `elem` lst = lst
    | otherwise     = el:lst
