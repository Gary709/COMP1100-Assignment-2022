{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AITests where

import           AI
import           Fanorona
import           Testing

testBoard :: Board
testBoard =
  [ [ Piece Player1, Empty, Empty ]
  , [ Empty, Empty, Empty, Empty ]
  , [ Empty, Empty, Piece Player2 ]
  , [ Empty, Piece Player1,Empty]
  ]

  --([ Piece Player1, Empty, Empty ], [ Empty, Empty, Empty, Empty ], [ Empty, Empty, Piece Player2 ], [ Empty, Piece Player1,Empty])

testGS :: GameState
testGS = State (Turn Player1) None (3,3) testBoard []


things :: Rose Int
things = 
    Rose 1 [
        Rose 2 [
            Rose 3 [], Rose 3 []
        ],
        
        Rose 3 [
            Rose 5 [
                Rose 5 [], Rose 7 []
            ],
            Rose 6 [
                Rose 45 [], Rose 65 [], Rose 42 []
            ]
        ],
        
        Rose 45 [
            Rose 12 [
                Rose 65 [], Rose 6 []
            ],
            Rose 3 [],
            Rose 2 []
        ],

        Rose 34 [
            Rose 3 [
                Rose 7 [], Rose 0 []
            ],
            Rose 1 [
                Rose 3 [], Rose 5 []
            ],
            Rose 8 [
                Rose 77 []
            ]
        ]
    ] 


aiTests :: Test
aiTests = TestGroup "AI"
  [ unMaybeTest
  , bestIndexValueTest
  , heuristicTest
  , allLegalMoveTest
  , removeSndTest
  , removeFstTest
  , heuristicAiTest
  , unMaybeStateTest
  , maxTreeTest
  , bestLegalResultTest
  , getBestResultTest
  , findDeleteTest
  , findDeleteTest1

  ]

-- ｜test unMaybe function transfer maybe to normal
unMaybeTest :: Test
unMaybeTest = Test "Test unMaybe is working or not"
  (assertEqual (unMaybe (Just 3)) (3:: Int))

bestIndexValueTest :: Test
bestIndexValueTest = Test "the highest value"
  (assertEqual (bestIndexValue [1,2,3]) (2::Int))

-- | test heuristic function in greedy
heuristicTest :: Test
heuristicTest = Test "Test heu function output"
  (assertEqual (heuristic1 (Turn Player1) testGS)(2:: Int))

-- | test heuristic function in ai
heuristicAiTest :: Test
heuristicAiTest = Test ("Test default ai's heu funciton")
  (assertEqual (heuristicAi (Turn Player1) testGS)(1:: Int))

-- | test all legal move
allLegalMoveTest :: Test
allLegalMoveTest = Test "all legal move "
  (assertEqual (allLegalMove testGS) 
  ([Move Approach (Location 0 0) (Location 1 1)]  :: [Move]))

-- | test to find the all gamestate 
unMaybeStateTest :: Test
unMaybeStateTest = Test "unMaybeState Test it will return a list of GameState"
  (assertEqual (unMaybeState COMP1100 testGS)
  ([State (GameOver (Winner Player1)) None (3,3) 
    [[Empty,Empty,Empty],[Empty,Piece Player1,Empty,Empty]
      ,[Empty,Empty,Empty],[Empty,Piece Player1,Empty]] []] :: [GameState]))

-- | helper function test to find the list of second value
removeSndTest :: Test
removeSndTest = Test"it will only keep fst value"
  (assertEqual (removeSnd [(2,3),(4,5),(6,7)])
  ([2,4,6] :: [Integer]))

-- | helper function test to find the list of fst value
removeFstTest :: Test
removeFstTest = Test"it will only keep the list of snd value"
  (assertEqual (removeFst [(2,3),(4,5),(6,7)])
  ([3,5,7] :: [Integer]))

-- | test the algorithm to find the maximum list
maxTreeTest :: Test
maxTreeTest = Test "Test Alpha Beta Tree"
  (assertEqual (maxTree things)
  ([(3,0)] :: [(Int,Score)]))

bestLegalResultTest :: Test
bestLegalResultTest = Test "output of best legal move"
  (assertEqual ( bestLegalResult testGS)
  (0 :: Score))

-- | test the best result
getBestResultTest :: Test
getBestResultTest = Test "move for the best"
  (assertEqual (getBestResult testGS 0)
  (Move Approach (Location 0 0) (Location 1 1) :: Move))

-- | find the binary values if ignoriable return 1 else 0
findDeleteTest :: Test
findDeleteTest = Test"the list can be ignore "
  (assertEqual (findDelete 3 [2,3,4]) (1 :: Int))

findDeleteTest1 :: Test
findDeleteTest1 = Test "the list cannot be ignore"
  (assertEqual (findDelete 3 [4,5,6]) (0 :: Int))


