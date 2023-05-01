module Automata where

import CodeWorld

-- | A 'Grid' of some cell type c consists of a pair of positive ints
-- describing its width and height, along with a list of cs that is
-- exactly (width * height) long.
data Grid c = Grid Int Int [c]

-- | Type alias for grid coordinates. This makes it clear when we are
-- talking about grid coordinates specifically, as opposed to some
-- other pair of integers.
type GridCoord = (Int, Int)


-- | Type of cells used in QR World.
data QRCell = Alive | Dead
    deriving(Show,Eq)

-- |  return the “next” type of QR cell
cycleQR :: QRCell -> QRCell
cycleQR  q = case q of
    Alive -> Dead
    Dead -> Alive
-- | Draw a singal cell in rectangle
renderQR :: QRCell -> Picture
renderQR Dead = coloured black $ rectangle 1 1
renderQR Alive = coloured blue $ solidRectangle 1 1

-- | count next generation QR World from the given algorithm 
nextGenQR :: Grid QRCell -> Grid QRCell
nextGenQR (Grid x y grid) = Grid x y 
  [evolutionQR cell (Grid x y grid) 
  | cell <- (zip grid (allCoords x y) )]
  

-- | the four cells around that coordinate: one step up, down, left, and right)
--  ideas from allCords function
getNeighbourhood :: GridCoord -> [GridCoord]
getNeighbourhood (a,b) = [(x,y) | x <- [(a-1) .. (a+1)]
                    , y <- [(b-1) .. (b+1)]
                    , (x,y) /= (a-1,b-1)
                    , (x,y) /= (a+1,b+1)
                    , (x,y) /= (a-1,b+1)
                    , (x,y) /= (a+1,b-1)
                    , (x,y) /= (a,b)]

-- | Given a neigbourhood, how many cells are of some particular type
-- alive means True and dead means False 
-- neighbourhood courts -> cells
getNeighborCells :: [GridCoord] -> Grid QRCell -> [Bool]
getNeighborCells neighbors grid =map
-- \c means add variable c in scope
   (\c -> (statusNeighbour) $ get grid c ) neighbors 

-- find the status of a neighbour if Alive return 1
-- if dead return 0
statusNeighbour :: Maybe QRCell -> Bool 
statusNeighbour x
  | x == (Just Alive) = True
  | otherwise = False

-- calculate alive cell by using helper function count
cellsNeighborsAlive :: (QRCell, GridCoord) -> Grid QRCell -> Int
cellsNeighborsAlive (_,coordinate) grid = (count) $ 
  getNeighborCells (getNeighbourhood coordinate) grid

-- | calculate occurrences of True
count :: [Bool] -> Int
count = foldl (\i v -> if v then i + 1 else i) 0



-- | evolve the coord if <0 return error
evolveQR :: Int -> Grid QRCell -> Grid QRCell
evolveQR n grid
  | n == 0 = grid
  | n < 0 = error" n<0 is not valid"
  | otherwise = evolveQR (n-1) (nextGenQR grid)

-- ｜ evolution of the QRcell from the given algorithm
evolutionQR :: (QRCell, GridCoord) -> Grid QRCell -> QRCell
-- if a neighbour cell is alive
evolutionQR (Alive,(a,b)) grid = case cellsNeighborsAlive (Alive, (a,b)) grid of
  3 -> Alive
  2 -> Alive
  _ -> Dead
-- if a neighbour cell is dead
evolutionQR (Dead, (a,b)) grid = case cellsNeighborsAlive(Dead,(a,b)) grid of
  2 -> Alive
  4 -> Alive
  _ -> Dead

  
-- | pick out the cell that the user is changing
get :: Grid c -> GridCoord -> Maybe c
get (Grid x y c) (a,b)
   --(x*y)+a is to index the output elemet
  | isGridExis (Grid x y c) (a,b)  = Just (c !!((x*b)+a) )
  | otherwise   = Nothing

 -- helper function for get, Find Grid exists or not 
isGridExis ::Grid c -> GridCoord -> Bool
isGridExis (Grid x y _) (a,b)
  | x <= a = False -- a b must greater than the width and length x y
  | y <= b = False
  | a < 0 = False -- a b <0 not in s scope
  | b < 0 = False
  | otherwise = True


allCoords :: Int -> Int -> [GridCoord]
allCoords x y
  | x>0 && y >0 = [(a,b) | b <- [0..(y-1)] , a <- [0..(x-1)]]
  | otherwise = error"this is not valid"