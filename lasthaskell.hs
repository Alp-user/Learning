module PE3 where

data Cell = SpaceCraft Int | Sand | Rock Int | Pit deriving (Eq, Read, Show)

type Grid = [[Cell]]
type Coordinate = (Int, Int)

data Move = North | East | South | West | PickUp | PutDown deriving (Eq, Read, Show)

data Robot = Robot { name :: String,
                     location :: Coordinate,
                     capacity :: Int,
                     energy :: Int,
                     storage :: Int } deriving (Eq, Read, Show)
------------------------------------Debug Examples-----------------------------------------
glorobot = Robot {name = "Alpsu", location = (0,0), capacity = 3, energy = 50, storage = 0}
glogrid = [[Rock 2,Sand,Sand,Sand,Rock 2],[Rock 0,Sand,SpaceCraft 1,Sand,Sand],[Rock 2,Rock 4,Sand,Pit,Sand]]
-------------------------------------------------------------------------------------------
--------------------------------- DO NOT CHANGE ABOVE -------------------------------------
------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
-------------------------------------------------------------------------------------------
-------------------------------------- PART I ---------------------------------------------

isInGrid :: Grid -> Coordinate -> Bool
isInGrid [] _ = False
isInGrid grid (cx,cy)  
    | (x >= 0 && y >= 0) && (cx>= 0 && cy >= 0) = True
    | otherwise = False 
    where 
        (a,b) = bottomright(grid)
        (x,y) =  (a-cx,b-cy)

llen [] = 0
llen (a:b) = 1 + llen b
bottomright grid = ( llen(innergr)-1,llen(grid) -1) where (innergr:_) = grid
-------------------------------------------------------------------------------------------

totalCount :: Grid -> Int
totalCount grid = findsum (lstcount grid) 0 

lstcount  grid = [x|xs<-grid,(Rock x)<-xs]
findsum [] s = s
findsum (a:b) s = findsum b (s+a)

-------------------------------------------------------------------------------------------

coordinatesOfPits :: Grid -> [Coordinate]
coordinatesOfPits gr = myquicksort (helperPit gr (0,0))
helperPit [] _  = [] 
helperPit (a:b) (y,x)  = (innerwork a x)++(helperPit b (y+1,0))
    where
        innerwork [] _ = [] 
        innerwork ((Pit):b) x  =(x,y):(innerwork b (x+1))
        innerwork (a:b) x = innerwork b (x+1)

mycompare (x,y) (x2,y2)
    | x < x2 = True
    | x == x2 && y < y2 = True
    | otherwise = False
myquicksort [] = []
myquicksort (a:b) = smalllst ++[a]++ bglst where
    smalllst = [x|x<-b, mycompare x a]
    bglst = [x|x<-b, not(mycompare x a)]
-------------------------------------------------------------------------------------------

tracePath :: Grid -> Robot -> [Move] -> [Coordinate]
tracePath grid robot moves = helperPath grid robot moves (bottomright grid)

helperPath  grid robot [] _ = []  
helperPath  grid robot (a:b) (mx,my)
    | energy == 0 = (location):(helperPath grid robot b (mx,my))
    | inList pitcord location = (location):(helperPath grid robot b (mx,my))
    | a == North && y>0 = (x,y-1):(helperPath grid (Robot n (x,y-1) capacity (energy-1) storage) b (mx,my))
    | a == South && y<my= (x,y+1):(helperPath grid (Robot n (x,y+1) capacity (energy-1) storage) b (mx,my))
    | a == East && x<mx= (x+1,y):(helperPath grid (Robot n (x+1,y) capacity (energy-1) storage) b (mx,my))
    | a == West && x>0 = (x-1,y):(helperPath grid (Robot n (x-1,y) capacity (energy-1) storage) b (mx,my))
    | a == PickUp = if(energy < 5) then ((location):(helperPath grid (Robot n location capacity 0 storage) b (mx,my))) else ((location):(helperPath (pickupform grid location True) (Robot n location capacity (energy -5) (if storage == capacity then storage else (storage+1))) b (mx,my)))
    | a == PutDown = (x,y):(helperPath (pickupform grid location False) (Robot n location capacity (energy-3) (if storage /= 0 then (storage -1) else storage))b (mx,my))
    |otherwise = (location):(helperPath grid robot b (mx,my))
    where
        (Robot n location capacity energy storage) = robot
        pitcord = coordinatesOfPits grid
        (x,y) = location
        
pickupform [] _ _ = []
pickupform (a:b) (x,y) flag = (innerwork a (x,y) flag):(pickupform b (x,y-1) flag) 
    where
        innerwork [] _ _ = []
        innerwork ((Rock a):b) (0,0) True = (Rock (if a == 0 then 0 else (a-1))):(innerwork b (x-1,y) flag) where 
        innerwork ((SpaceCraft a):b) (0,0) False = (SpaceCraft (a+1)):(innerwork b (x-1,y) flag) where 
        innerwork (a:b) (0,0) False = (a):(innerwork b (x-1,y) flag) where 
        innerwork (a:b) (c,d) flag = a:innerwork b (c-1,d) flag

inList [] _ = False
inList (a:b) element = if(element == a) then True else inList b element
------------------------------------- PART II ----------------------------------------------

energiseRobots :: Grid -> [Robot] -> [Robot]
energiseRobots grid robots = innerwork robots
    where
        innerwork  [] = []
        innerwork  ((Robot name location capacity energy storage):b)
            | (energy + chargeval) > 100 = ((Robot name location capacity 100 storage):(innerwork b))
            | otherwise = ((Robot name location capacity (energy + chargeval) storage):(innerwork b))
                where
                    chargeval = calcgain location sloc
                    (sloc:_) = helpenergy grid (0,0)
        

helpenergy [] _ = []
helpenergy (a:b) (y,x)  = (innerwork a x)++(helpenergy b (y+1,0))
    where
        innerwork [] _ = [] 
        innerwork ((SpaceCraft a):b) x  =(x,y):(innerwork b (x+1))
        innerwork (a:b) x = innerwork b (x+1)


calcgain (rx,ry) (sx,sy) = let value = 100 - (abs(rx-sx)+abs(ry-sy))*20 in
    if ( value < 0) then 0 else value

-------------------------------------------------------------------------------------------

applyMoves :: Grid -> Robot -> [Move] -> (Grid, Robot)
applyMoves grid robot moves = helpagain grid robot moves (bottomright grid)


helpagain  grid robot [] _ = (grid, robot)  
helpagain  grid robot (a:b) (mx,my)
    | energy == 0 = (helpagain grid robot b (mx,my))
    | inList pitcord location && (a==North||a== South||a==East||a==West)=(helpagain grid (Robot n location capacity (energy-1) storage) b (mx,my))
    | inList pitcord location && (a==PickUp)=(helpagain grid (Robot n location capacity (energy-5) storage) b (mx,my))
    | inList pitcord location && (a==PutDown)=(helpagain grid (Robot n location capacity (energy-3) storage) b (mx,my))
    | a == North && y>0 = (helpagain grid (Robot n (x,y-1) capacity (energy-1) storage) b (mx,my))
    | a == South && y<my= (helpagain grid (Robot n (x,y+1) capacity (energy-1) storage) b (mx,my))
    | a == East && x<mx= (helpagain grid (Robot n (x+1,y) capacity (energy-1) storage) b (mx,my))
m   | a == West && x>0 = (helpagain grid (Robot n (x-1,y) capacity (energy-1) storage) b (mx,my))
    | (a == West && x== 0) || (a == East && x== mx) || (a == North && y== 0) || (a == South && y == my) = (helpagain grid (Robot n location capacity (energy-1) storage) b (mx,my))
    | a == PickUp = if(energy < 5) 
        then ((helpagain grid (Robot n location capacity 0 storage) b (mx,my))) 
        else 
            if storage == capacity 
                then ((helpagain grid (Robot n location capacity (energy -5) storage )) b (mx,my))
                else ((helpagain newgrid (Robot n location capacity (energy -5) (if newgrid == grid then storage else (storage+1)) )) b (mx,my))
    | a == PutDown = if(energy<3) then(helpagain grid (Robot n location capacity 0 storage) b (mx,my)) else (helpagain (pickupform grid location False) (Robot n location capacity (energy-3) (if storage /= 0 then (storage -1) else storage))b (mx,my))
    |otherwise = (helpagain grid robot b (mx,my))
    where
        (Robot n location capacity energy storage) = robot
        pitcord = coordinatesOfPits grid
        (x,y) = location
        newgrid = (pickupform grid location True)
