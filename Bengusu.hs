
data GridTree a = EmptyTree | GridTree a [GridTree a] deriving (Show, Eq)


findNode :: Eq a => a -> GridTree a -> (Int, Int) -> a
findNode x EmptyTree _ = x
findNode x (GridTree val children) (0,0) = val
findNode x (GridTree val children ) (0,index) = x
findNode x (GridTree val children ) (depth,index) = inIt (fiterator findChildList children (depth-1)) index x

findLocationOf :: Eq a => a -> GridTree a -> (Int, Int)
findLocationOf wvalue (GridTree value children) = findLocRec (GridTree value children) wvalue 0

insertNode :: Eq a => a -> GridTree a -> (Int, Int) -> GridTree a
insertNode wvalue tree (x',y') = let
    (GridTree value children) = tree
    treedepth = findDepth tree 0
    local1 EmptyTree check = GridTree wvalue []
    local1 tree "not row" = outsidecase2 wvalue tree (x',lengthhelper 0 (fiterator findChildList children (x'-1))-1) 0 tree
    local1 tree "not column" = outsidecase1 wvalue tree (treedepth+1,y') 0 tree
    local1 tree "inside" = if ((llen tree) >= x') && (y' == 0) then insidefirstinsert tree wvalue (x',y') 0 else insidesecondinsert wvalue tree (x',y') 0 tree
    in local1 tree (typechecker tree (x',y'))

llen EmptyTree = 0
llen (GridTree _ []) = 1
llen (GridTree _ (a:b)) = 1 + llen a


insidefirstinsert (GridTree value (child:restchildren)) wvalue (x,y) d = if d == x-1 then GridTree value (GridTree wvalue []:(child:restchildren)) else GridTree value (insidefirstinsert child wvalue (x,y) (d+1):restchildren)

insidesecondinsert :: (Num p, Eq b, Num b, Eq p, Eq a) => a -> GridTree a -> (p, b) -> p -> GridTree a -> GridTree a
insidesecondinsert wvalue (GridTree value children)  (x,y) d tree 
    | y  == 0 && d == x-1 && findColumn list (GridTree value children) d == 0 = GridTree value (GridTree wvalue []:children )
    |children == [] = GridTree value []
    |otherwise =  GridTree value (insidesecondrec (d+1 == x) children insidesecondinsert (x,y) (d+1) wvalue tree)
        where
           (GridTree value list) = tree

insidesecondrec check list function (x,y) d wvalue tree = if check == True then local1 list else local2 list where
    local2 [] = []
    local2 (first:rest) = function wvalue first (x,y) d tree : local2 rest

    local1 [] = []
    local1 (first:rest) | y== 0 = if findColumn children first d == 0 then  function wvalue (GridTree wvalue []) (x,y) d tree : function wvalue first (x,y) d tree : local1 rest else function wvalue first (x,y) d tree : local1 rest
                        |findColumn children first d == y-1 = function wvalue first (x,y) d tree : function wvalue (GridTree wvalue []) (x,y) d tree : local1 rest
                        |otherwise = function wvalue first (x,y) d tree : local1 rest
    (GridTree value children) = tree



outsidecase1 wvalue (GridTree value children) (x,y) d tree = if children == [] then (if y==0 && d == x-1 && findColumn list (GridTree value children) d == 0 then GridTree value [GridTree wvalue []] else GridTree value children ) else GridTree value (outsidecase1rec children (x,y) (d+1) wvalue tree outsidecase1) where (GridTree xvalue list) = tree
outsidecase1rec children (x,y) d wvalue tree function = [function wvalue element (x,y) d tree | element <- children]


outsidecase2 wvalue (GridTree value children) (x,y) d tree = if children == [] then GridTree value children else GridTree value (outsidecase2rec (d+1 == x) children outsidecase2  (x,y)  (d+1) wvalue tree)
outsidecase2rec check list function (x,y) d wvalue tree = if check == True then local1 list else local2 list where
    local2 [] = []
    local2 (first:rest) = function wvalue first (x,y) d tree : local2 rest

    local1 [] = []
    local1 (first:rest) | findColumn children first d == y =  function wvalue first (x,y) d tree : function wvalue (GridTree wvalue []) (x,y) d tree : local1 rest
                        |otherwise = function wvalue first (x,y) d tree : local1 rest
    (GridTree value children) = tree

findChildList [] = []
findChildList (EmptyTree:restparents) = findChildList restparents
findChildList ((GridTree _ []):restparents) = findChildList restparents
findChildList ((GridTree value (firstchild:restchildren):restparents))  | firstchild == EmptyTree = findChildList ((GridTree value restchildren):restparents)
                                                                        | restchildren == [] = firstchild:findChildList restparents                                                                   | otherwise = firstchild:(findChildList (GridTree value restchildren:restparents))
findLocRec :: (Num a, Eq a, Eq t) => GridTree t -> t -> a -> (a, a)
findLocRec EmptyTree _ _ = (-1,-1)
findLocRec (GridTree value children ) wvalue 0  = if value == wvalue then (0,0) else findLocRec (GridTree value children) wvalue 1
findLocRec (GridTree value children ) wvalue depth | firstdfs == [] = (-1,-1)
                                                   | otherwise = if dfs == -1 then  findLocRec (GridTree value children) wvalue (depth+1) else (depth,dfs)
                                                  where
                                                   firstdfs = fiterator findChildList children (depth-1)
                                                   dfs = findIndex 0 wvalue firstdfs

maximumelement [] s = s
maximumelement (a:b) s = if a>s then maximumelement b a else maximumelement b s


typechecker ::Eq a => GridTree a -> (Int, Int) -> String
typechecker tree (x,y) | findInsideRec '~' tree (x,y) == True = "inside"
                       | findInsideRec '~' tree (x,y) == False && findDepth tree 0 < x = "not column"
                       | findInsideRec '~' tree (x,y) == False && findDepth tree 0 >= x = "not row"

findDepth (GridTree value children) depth = if children == [] then depth else maximumelement (depthhelper findDepth children (depth+1)) 0
depthhelper function children depth = [function child depth | child<-children]

findColumn tree wvalue d = isInlocal wvalue (fiterator findChildList tree (d-1)) 0
isInlocal :: (Eq t1, Num t2) => t1 -> [t1] -> t2 -> t2
isInlocal  wvalue (a:b) d  | (a:b) == [] = -1
                           | otherwise  = if a == wvalue then d else isInlocal wvalue b  (d+1)
inIt [] index x = x
inIt ((GridTree value children):rest) 0 x = value
inIt ((GridTree value children):rest) index x = inIt rest (index-1) x

isIn [] _ x = False
isIn ((GridTree value children):rest) 0  x = True
isIn ((GridTree value children):rest) n  x = isIn rest (n-1) x


findInsideRec x EmptyTree _ = False
findInsideRec x (GridTree val children) (0,0) = True
findInsideRec x (GridTree val children ) (0,index) = False
findInsideRec x (GridTree val children ) (depth,index) = isIn (fiterator findChildList children (depth-1)) index x

findIndex index wvalue [] = -1
findIndex index wvalue ((GridTree value children):rest) | value == wvalue = index
                                                        | otherwise = findIndex (index+1) wvalue rest
fiterator f childrenlist 0 = childrenlist
fiterator f childrenlist n = fiterator f (f childrenlist) (n-1)

lengthhelper l []  = l
lengthhelper l (a:b)  = lengthhelper (l+1) b

listlength (a:b) = lengthhelper 0






t = GridTree 1 [
    GridTree 2 [
        GridTree 3 [
            GridTree 4 []],
        GridTree 5 []],
    GridTree 6 [
        GridTree 7 [
            GridTree 8 [
                GridTree 9 [],
                GridTree 10 [],
                GridTree 11 []]]]]
z = [GridTree 'C' [],GridTree 'D' [],GridTree 'E' [],GridTree 'F' []]
