data GridTree a = EmptyTree | GridTree a [GridTree a] deriving (Show, Eq)

n = GridTree "List" [GridTree "Tree" [GridTree "Binary" []],GridTree "Maybe" [],GridTree "Table" [GridTree "Search" [GridTree "Hash" [],GridTree "Heap" []]]]
t = GridTree 'A' [
        GridTree 'B' [
            GridTree 'C' [
                GridTree 'D' []],
            GridTree 'E' []],
        GridTree 'F' [
            GridTree 'G' [
                GridTree 'H' [
                    GridTree 'I' [],
                    GridTree 'J' [],
                    GridTree 'K' []]]]]


findNode :: Eq a => a -> GridTree a -> (Int, Int) -> a
findNode e treee (lc,rc) 
    | treee == EmptyTree = e
    | lc == 0 && rc == 0 = val
    | lc == 0 && rc > 0 = e
    | otherwise = checkvals (manytimes  lst (lc - 1)) rc e
    where
        (GridTree val lst) = treee

iterfunc f lst 0 = lst
iterfunc f lst n = iterfunc f (f lst) (n-1)


concatchild lst   
    | lst == [] = []
    | secondlst == [] = concatchild b
    | otherwise = c:(concatchild ((GridTree val d):b))
    where
        (a:b) = lst
        (GridTree val secondlst) = a
        (c:d) = secondlst 

manytimes :: Eq a => [GridTree a] -> Int -> [GridTree a]
manytimes = iterfunc concatchild

checkvals [] _ emp = emp
checkvals ((GridTree val _):b) 0 emp = val
checkvals (((GridTree val _)):b) rn emp = checkvals b (rn-1) emp

findTode :: Eq a =>  GridTree a -> (Int, Int) -> Bool 
findTode  treee (lc,rc) 
    | treee == EmptyTree = False
    | lc == 0 && rc == 0 = True
    | lc == 0 && rc > 0 = False 
    | otherwise = boolcheckvals (manytimes  lst (lc - 1)) rc  
    where
        (GridTree val lst) = treee
    
boolcheckvals [] _  = False
boolcheckvals ((GridTree val _):b) 0 = True
boolcheckvals (((GridTree val _)):b) rn  = boolcheckvals b (rn-1) 

checkplace [] _ y = -1 
checkplace (((GridTree val _)):b) rn y = if val == rn then y else checkplace b rn (y+1) 

findLocationOf ::Eq a => a -> GridTree a -> (Int, Int)
findLocationOf val treee = innerwork val treee 0 where
    innerwork _ EmptyTree _ = (-1,-1)
    innerwork val (GridTree v2 l) 0 = if val == v2 then (0,0) else innerwork val (GridTree v2 l) 1
    innerwork val (GridTree v2 l) x = let trial = manytimes l (x-1) in
        if trial == [] then (-1,-1) else (let anothertrial = checkplace trial val 0 in  if anothertrial == -1 then innerwork val (GridTree v2 l) (x+1) else (x,anothertrial))



insertNode :: Eq a => a -> GridTree a -> (Int, Int) -> GridTree a
insertNode char gr (lc,rc) = innerwork (whichcase gr (lc,rc) ) gr 
    where
        maxdepth = depthtree gr 0
        (GridTree _ ls) = gr
        innerwork casebool gr
            |gr == EmptyTree = GridTree char []
            |casebool == 2 = if (llen gr >= lc)&&(rc == 0) then zeroformer char gr (lc,rc) 0 else formselfan char gr (lc,rc) 0 gr
            |casebool == 1 = depthcase char gr (maxdepth+1,0) 0 gr
            |casebool == 0 = rowcase char gr (lc,reallen (manytimes ls (lc-1))-1) 0 gr
            |otherwise = EmptyTree

reallen [] = 0
reallen (a:b) = 1 + reallen b

llen EmptyTree = 0
llen (GridTree _ []) = 1
llen (GridTree _ (a:b)) = 1 + llen a

zeroformer char (GridTree val lst) (depth,col) curdep  
    | curdep == depth -1 = GridTree val (GridTree char []:lst)
    | otherwise = GridTree val ((zeroformer char a (depth,col) (curdep+1)):b)
    where (a:b) = lst

depthcase char (GridTree val lst) (depth,col) curdep gr 
    | lst == [] && col == 0 && (curdep == depth - 1) && ((columndex ls (GridTree val lst) curdep) == 0) = GridTree val [GridTree char []]  
    | lst == [] = GridTree val lst
    | otherwise = GridTree val (depthbuilder depthcase lst (depth,col) (curdep+1) char gr)
    where (GridTree _ ls) = gr

depthbuilder f lst (x,y) curdep char gr = [ f char n (x,y) curdep gr | n<-lst]

formselfan char (GridTree val lst) (depth,col) curdep gr 
    | col == 0 && (curdep == depth -1) && ((columndex ls (GridTree val lst) curdep) == 0) = GridTree val (GridTree char []:lst)
    | lst == []  = GridTree val []
    | otherwise = GridTree val (normalbuider formselfan lst (if (curdep +1) == depth then True else False) (depth,col) (curdep+1) char gr)
    where (GridTree _ ls) = gr


rowcase char (GridTree val lst) (depth,col) curdep gr = if lst == [] then GridTree val lst else GridTree val (rowbuilder rowcase lst (if (curdep+1) == depth then True else False) (depth,col) (curdep+1) char gr) 

rowbuilder f lst flagged (x,y) curdep char gr
    | flagged == True = innerwork lst
    | flagged == False = innerwork2 lst
    where
        (GridTree _ ls) = gr
        innerwork [] = []
        innerwork (a:b) = if (columndex ls a curdep) == y
                          then  (f char a (x,y) curdep gr) : (f char (GridTree char []) (x,y) curdep gr) : (innerwork b)
                          else (f char a (x,y) curdep gr) : (innerwork b)         
        innerwork2 [] = []
        innerwork2 (a:b) = (f char a (x,y) curdep gr) : (innerwork2 b)


normalbuider f lst flagged (x,y) curdep char gr
    | flagged == True = innerwork lst
    | flagged == False = innerwork2 lst
    where
        (GridTree _ ls) = gr
        innerwork [] = []
        innerwork (a:b) = if y == 0 then (if (columndex ls a curdep) == 0
                              then  (f char (GridTree char []) (x,y) curdep gr) : (f char a (x,y) curdep gr) :  (innerwork b)
                              else (f char a (x,y) curdep gr) : (innerwork b)) else (if (columndex ls a curdep) == (y-1)
                                  then   (f char a (x,y) curdep gr) : (f char (GridTree char []) (x,y) curdep gr) : (innerwork b)
                                  else (f char a (x,y) curdep gr) : (innerwork b))
        innerwork2 [] = []
        innerwork2 (a:b) = (f char a (x,y) curdep gr) : (innerwork2 b)

depthtree (GridTree val lst) depth = if lst == [] then depth else mymax (mappedformselfan2 depthtree lst (depth+1))
mappedformselfan2 f lst depth = [f x depth| x<-lst]

mymax (a:b) = innerwork b a where
    innerwork [] s = s
    innerwork (c:d) s = if c>s then innerwork d c else innerwork d s

columndex looklist curval depth  = let checklist = manytimes looklist (depth-1) in
    insideit checklist curval 0 where
        insideit [] curval x = -1
        insideit (a:b) curval x = if (a == curval) then x else insideit b curval x+1
    
whichcase gr (x,y) = innerwork (depthtree gr 0) (findTode gr (x,y)) where
    innerwork maxdepth signifier
        | signifier == True = 2 
        | signifier == False && (maxdepth < x) = 1
        | signifier == False && (maxdepth >= x) = 0

