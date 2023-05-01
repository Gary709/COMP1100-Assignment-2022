module AutomataTest where

import Automata
import Testing
import TestPatterns

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked. You should
-- remove these examples when submitting the assignment.
tests :: [Test]
tests =
  [ allCoordsTest
  , allCoordsTest1
  , getNeighbourhoodTest
  , getNeighbourhoodTest1
  , getNeighborCellsTest
  , getNeighborCellsTest1
  , toQRTest
  , statusNeighbourTest
  , evolutionQRTest
  , cellsNeighborsAliveTest
  , isGridExisTest
  , isGridExisTest1
  , isGridExisTest2
  , cellsNeighborsAliveTest1
  ]

-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your code, you can use this space to say
-- things like "the next state for a cell with 3 live neighbours is
-- 'Alive'".
--


-- | Testing allCoords function from the example given on instruction
allCoordsTest :: Test
allCoordsTest = Test
  "allCoords 3 2 == [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]" 
  (assertEqual (allCoords 3 2) 
  ([(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]::[GridCoord]))

allCoordsTest1 :: Test
allCoordsTest1 = Test 
  "allCoords 4 3 == [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]"
  (assertNotEqual (allCoords 4 3)
  ([(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]::[GridCoord]))

-- | Testing toQR function
toQRTest :: Test
toQRTest = Test "toQR 'A' == Alive" 
              (assertEqual (toQR 'A') (Automata.Alive))

-- | Testing getNeighbourhood function it should return neighbour coordinate
getNeighbourhoodTest :: Test
getNeighbourhoodTest = Test 
  "getNeighbourhood (2,3)==[(1,3),(2,2),(2,4),(3,3)]" 
  (assertEqual (getNeighbourhood (2,3)) 
  ([(1,3),(2,2),(2,4),(3,3)]::[GridCoord]))

getNeighbourhoodTest1 :: Test
getNeighbourhoodTest1 = Test "getNeighbourhood (3,3) == [(1,2),....,(3,4)]" 
  (assertNotEqual (getNeighbourhood (3,3)) 
  ([(1,2),(1,3),(1,4),(2,2),(2,4),(3,2),(3,3),(3,4)]::[GridCoord]))

-- |  test calculate the alive cells in a list
getNeighborCellsTest :: Test
getNeighborCellsTest = Test "2 alive cells"
  (assertEqual (getNeighborCells [(2,3),(2,2),(1,2)] 
  (Grid 6 7[Alive,Alive,Dead,Alive,Alive,Alive,Alive
  ,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Alive,Dead
  ,Alive,Dead,Alive,Dead,Alive,Dead,Alive]))
  ([True,True,False]::[Bool]))

getNeighborCellsTest1 :: Test
getNeighborCellsTest1 = Test "2 alive cells"
  (assertNotEqual (getNeighborCells [(2,3),(2,2)] 
  (Grid 6 7[Alive,Alive,Dead,Alive,Alive,Alive,Alive
  ,Dead,Alive,Dead,Alive,Dead,Dead,Dead,Alive,Dead
  ,Alive,Dead,Alive,Dead,Alive,Dead,Alive]))
  ([True,True,False]::[Bool]))

-- ｜ Test statisNeighbour have a correct output
statusNeighbourTest :: Test
statusNeighbourTest = Test " statusNeighbour Just(Alive) == 1" 
  (assertEqual (statusNeighbour (Just Alive))
  (True :: Bool))

evolutionQRTest :: Test
evolutionQRTest = Test" evolutionQR function"
  (assertEqual (evolutionQR (Alive, (3, 4)) 
  (Grid 1 2 [Alive, Alive, Alive]))
  (Dead :: QRCell))

-- | test how many Alive neighbor cells
cellsNeighborsAliveTest :: Test
cellsNeighborsAliveTest = Test "cells" 
  (assertEqual (cellsNeighborsAlive (Alive, (3, 4)) 
  (Grid 1 2 [Alive, Alive, Alive]))
  (0:: Int))

cellsNeighborsAliveTest1 :: Test
cellsNeighborsAliveTest1 = Test "alive cells"
  (assertEqual (cellsNeighborsAlive (Alive, (3, 4)) (Grid 5 6 [Alive
  , Dead, Alive, Dead, Alive, Dead, Alive, Dead, Alive, Dead, Alive
  , Dead, Alive, Dead, Alive, Alive, Dead, Alive, Alive, Dead, Alive
  , Alive, Dead, Alive, Alive, Dead, Alive, Alive, Dead, Alive, Alive
  , Dead, Alive, Alive, Dead, Alive, Alive, Dead, Alive, Alive, Dead
  , Alive, Alive, Dead, Alive, Alive, Dead, Alive]) )
  (2 :: Int))

-- | True situation result
isGridExisTest :: Test
isGridExisTest = Test "result == True"
  (assertEqual (isGridExis (Grid 3 4 []) (1,2))
  (True :: Bool))

-- x < a situation
isGridExisTest1 :: Test
isGridExisTest1 = Test "result == False"
  (assertEqual (isGridExis (Grid 3 4 [])(7,8))
  (False :: Bool))

-- | negative situation
isGridExisTest2 :: Test 
isGridExisTest2 = Test "negative input and resule == False"
  (assertEqual (isGridExis (Grid 3 4 []) ((-1),3))
  (False :: Bool))





