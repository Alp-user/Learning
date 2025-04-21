
data GridTree a = EmptyTree | GridTree a [GridTree a] deriving (Show, Eq)

findNode' ((x, y), h, ws, e) (GridTree d children)
    | h == x && w == y = ((x, y), h, (w+1):nws, d)
    | otherwise        = ((x, y), h, (w+1):nws, ne)
        where w   = if null ws then 0 else head ws
              cws = if null ws then [] else tail ws
              (_, _, nws, ne) = foldl findNode' ((x, y), h+1, cws, e) children

findNode :: a -> GridTree a -> (Int, Int) -> a
findNode e EmptyTree _ = e
findNode e t (x, y) = ne
    where (_, _, _, ne) = findNode' ((x, y), 0, [], e) t

findLocationOf' (e, h, ws, (x,y)) (GridTree d children)
    | e == d    = (e, h, ws, (h, w))
    | otherwise = (e, h, (w+1):nws, (nx, ny))
        where w   = if null ws then 0 else head ws
              cws = if null ws then [] else tail ws
              (_, _, nws, (nx, ny)) = foldl findLocationOf' (e, h+1, cws, (x, y)) children

findLocationOf :: (Eq a) => a -> GridTree a -> (Int, Int)
findLocationOf _ EmptyTree = (-1, -1)
findLocationOf e t =  (x, y)
    where (_, _, _, (x, y)) = findLocationOf' (e, 0, [], (-1, -1)) t

getRow (GridTree d children) 0 = [GridTree d children]
getRow (GridTree d children) w = foldl (++) [] (map ((flip getRow) (w-1)) children) 

vLevel t r
    | l == []   = vLevel t (r-1)
    | otherwise = l
    where l = getRow t r

findParent (end:[]) w = (end, w)
findParent ((GridTree d children):siblings) w
    | length children >= w = (GridTree d children, w)
    | otherwise            = findParent siblings (w - length children)

insertAtIndex elem lst index = take index lst ++ [elem] ++ drop index lst

addToNode elem t (h, w) = GridTree d (insertAtIndex (GridTree elem []) children nw)
    where level = vLevel t (h - 1)
          ((GridTree d children), nw) = findParent level w

buildTree (GridTree d children) b p = GridTree d finalChildren
    where builtChildren = map (\x -> buildTree x b p) children
          finalChildren = map (\x -> if x == p then b else x) builtChildren

insertNode :: Eq a => a -> GridTree a -> (Int, Int) -> GridTree a
insertNode elem EmptyTree _ = (GridTree elem [])
insertNode elem t (1, w) = addToNode elem t (1, w)
insertNode elem t (h, w) = buildTree t (addToNode elem t (h, w)) parent
    where level = vLevel t (h - 1)
          parent = fst (findParent level w)
