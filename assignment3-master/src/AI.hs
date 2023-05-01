{-|
Module      : AI
Description : AIs for Fanorona
Copyright   : (c) 2022 ANU and Your Name Here
License     : AllRightsReserved
-}
module AI where

import           Fanorona
import Data.List
import Data.Ord


-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up. 

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- his or her attention to marking.
ais :: [(String, AIFunc)]
ais = [ ("greedy1", NoLookahead (greedy COMP1100)),
        ("first", NoLookahead (firstLegalMove COMP1100)),
        ("greedy2", NoLookahead (greedy2 COMP1100)),
        ("default", WithLookahead (alphaAi COMP1100))
      ]

-- | A very simple AI, which passes whenever it can, and if not,
-- picks the first move returned by the 'legalMoves' function.
-- By default, this function is called on COMP1100 rules, and so
-- will never pass.
-- AIs can rely on the 'legalMoves' list being
-- non-empty; if there were no legal moves, the framework would have
-- ended the game.
firstLegalMove :: Course -> GameState -> Move
firstLegalMove course state = case applyMove course Pass state of
  Nothing -> head (legalMoves state)
  _ -> Pass

-- | find all valid legal move
allLegalMove :: GameState -> [Move]
allLegalMove state = legalMoves state

-- | reverse the Move and Gamestate in order to easier to use the map
--   which can be more efficency 
applymoveReverse :: Course -> GameState -> Move -> Maybe GameState 
applymoveReverse course state move = applyMove course move state

-- | apply the applyMove function find all gamestate
findMaybeState :: Course -> GameState -> [Maybe GameState]
findMaybeState course state = map (applymoveReverse course state) 
  (allLegalMove state)

-- | simple unMaybe function be used in next function
unMaybe :: Maybe a -> a
unMaybe maybeState = case maybeState of
  Just x -> x
  Nothing -> error"Nothing cannot unMaybe"

-- | remove maybe make the findMaybeState function in list
unMaybeState :: Course -> GameState -> [GameState]
unMaybeState course state = map (unMaybe) (findMaybeState course state)

-- | evaluate the largest value in order easier to use indexing 
bestIndexValue ::  Ord a => [a] -> Int
bestIndexValue = fst . maximumBy (comparing snd) . zip [0..]

-- | simple heuristic function consider the position of the countPieces
heuristic1 :: Turn -> GameState -> Int
heuristic1 (Turn Player1) state = fst (countPieces state)
heuristic1 (Turn Player2) state = snd (countPieces state)

-- This heuristic consider difference between player's pieces and opponent's 
-- pieces.
heuristic :: Turn -> GameState -> Int
heuristic t state = case t of
  Turn player -> case player of
    Player1 -> heuA (countPieces state)
    Player2 -> heuB (countPieces state)
  where heuA :: (Int,Int) -> Int
        heuA s
          | s == (0,0) = 0
          | fst(s) > snd(s) = fst(s)
          | otherwise       = fst(s) + snd(s)
        heuB :: (Int,Int) -> Int
        heuB s
          | s == (0,0) = 0
          | fst(s) < snd(s) = snd(s)
          | otherwise       = fst(s) + snd(s)

-- | greedy ai it's smarter than the first legal move ai
greedy :: Course -> GameState -> Move
greedy course state = case turn state of
  GameOver _ -> error"Game over!!!!!!!"
  Turn Player1 ->  (getBest (allLegalMove state) (helper1 course state))
  Turn Player2 ->  (getBest (allLegalMove state) (helper1 course state))

-- | greedy2 ai chooses best move by applying all the possible moves to 
-- present game state and then evaluating them to find one with highest amount
-- of difference between player and opponent. 
greedy2 :: Course -> GameState -> Move
greedy2 course state = case turn state of
  GameOver _ -> error"Game over!!!!!!!"
  Turn Player1 ->  (getBest (allLegalMove state) (helper course state))
  Turn Player2 ->  (getBest (allLegalMove state) (helper course state))

-- | Helper function apply move on gameState and then return heuristic value
--   of that gameState. helper1 is for greedy1 helper is for greedy2
helper :: Course -> GameState -> [Int]
helper c sta = map (heuristic t) (unMaybeState c sta)
  where
    t = turn sta 

helper1 :: Course -> GameState -> [Int]
helper1 c sta = map (heuristic1 t) (unMaybeState c sta)
  where
    t = turn sta 
-- | getBest compares list of Int and find the index of highest Int and return 
-- that move. Here, move represent all possible moves for given gameState
getBest :: [Move] -> [Int] -> Move
getBest move x = move !! bestIndexValue x


------trees and minmax, alpha beta pruning start

-- | Rose tree for the generations for lookahead moveset.
data Rose a = Rose a [Rose a]
  deriving (Eq)

-- | ignore these code these to help me visulized the tree
instance Show a => Show (Rose a) where
    show tree = pretty tree
        where
          pretty :: Show a => Rose a -> String
          pretty = unlines.layout
          layout :: Show a => Rose a -> [String]
          layout (Rose v []) = [show v]
          layout (Rose v children) = [show v] ++ concat 
            (map indent (map layout children))
          indent :: [String] -> [String]
          indent = map ("  "++)

-- | evaluate all gameTree in Rose GameState data type
gameTree :: GameState -> Rose GameState
gameTree state = Rose state (map gameTree(unMaybeState COMP1100 state))

-- | copied from the lab09 helper function to map the rose tree
roseMap :: (a -> b) -> Rose a -> Rose b
roseMap m (Rose x y) = case y of
    [] -> Rose (m x) []
    ys -> Rose (m x) (map (roseMap m) ys)

-- | It will transform the tree to the Rose tree of score
resultTree :: GameState -> Rose Int
resultTree state = roseMap 
  (heuristicAi player) (gameTree state)
  where player = turn state

-- | get using the heuristic funciton to find the result, winner return 1
-- and draw return 0
heuristicAi :: Turn -> GameState -> Int
heuristicAi t state = case t of
  Turn player -> case player of
    Player1 -> if fst (countPieces state) > snd (countPieces state) 
        then 1 else 0
    Player2 -> if snd (countPieces state) > fst (countPieces state) 
        then 1 else 0
  GameOver outcome  -> case outcome of
    Draw -> 0

-- | To using alpha beta pruning add the Score to mark what to index
  -- to find the best move
type Score = Int

-- | find the maximum Score and the maximum score
maximize :: Rose Int -> (Int, Score)
maximize t = (maximum tree1, maximum tree2)
  where
    tree1 = (removeSnd (miniTree t))
    tree2 = (removeFst (miniTree t))

minimize :: Rose Int -> (Int, Score)
minimize t = (minimum tree1, minimum tree2)
  where
    tree1 = (removeSnd (maxTree t))
    tree2 = (removeFst (maxTree t))

-- | remove the Score and return the scores for indexing
removeSnd :: [(a,a)] -> [a]
removeSnd lst = case lst of
  [] -> []
  (a,_) : xs -> a : removeSnd xs

-- | this function remove the score to find the actually Score for indexing
removeFst :: [(a,a)] -> [a]
removeFst lst = case lst of
  [] -> []
  (_,b) : xs -> b : removeFst xs

-- | this function find the minimal list of the tree
miniTree :: Rose Int -> [(Int, Score)]
miniTree t = case t of
  Rose a [] -> [(a,0)]
  Rose _ list -> findMaxs (map maxTree list)

-- | this function will evaluate the maximum of the list
findMaxs :: [[(Int,Score)]] -> [(Int,Score)]
findMaxs lst = case lst of
  [] -> []
  (x:xs) -> ((minimum (removeSnd x)),0): 
    (deleteMax 1 (minimum (removeSnd x)) xs)


-- | ignore and delete some unused branches
deleteMax :: Int -> Int -> [[(Int,Score)]] -> [(Int,Score)]
deleteMax marker posi lst = case lst of
  [] -> []
  x:xs -> if findDelete posi (removeSnd x) == 1 
      then deleteMax (marker + 1) posi xs 
      else (minimum (removeSnd x),marker) : (deleteMax (marker + 1) 
        (minimum (removeSnd x)) xs)

-- | Returns a list of binary values representing can be true (1) and otherwise (0)
--  by comparing maximum part
findDelete :: Int -> [Int] -> Int
findDelete int1 int2 = case int2 of
  []  -> 0
  x : xs -> if int1 >= x 
            then 1 
            else findDelete int1 xs

-- | find maximum tree lst and ignore the minimum branches
maxTree :: Rose Int -> [(Int,Score)]
maxTree t = case t of
    Rose a [] -> [(a,0)]
    Rose _ list -> findMins (map miniTree list)

--  |find the min of the list 
findMins :: [[(Int,Score)]] -> [(Int,Score)]
findMins lst = case lst of
  [] -> []
  (x:xs) -> ((maximum (removeSnd x)),0): 
    (deleteMin 1 (maximum (removeSnd x)) xs)
   

-- | ignore and delete some unused branches
deleteMin :: Int -> Int -> [[(Int,Score)]] -> [(Int,Score)]
deleteMin score pot lst = case lst of
  [] -> []
  x:xs -> if findDeleteMin pot (removeSnd x) == 1 
    then deleteMin (score + 1) pot xs 
    else (maximum (removeSnd x),score) : (deleteMin (score + 1) 
      (maximum (removeSnd x)) xs)

-- | Returns a list of binary values representing can be delete (1) and otherwise (0)
--  by comparing minimum part
findDeleteMin :: Int -> [Int] -> Int
findDeleteMin int1 int2 = case int2 of
  []  -> 0
  x : xs -> if int1 <= x 
            then 1 
            else findDeleteMin int1 xs


-- | It will return the Best value so far and marker of a pruning tree
bestLegalResult :: GameState -> Score
bestLegalResult state = snd(maximize (pruneDepth 5 (resultTree state)))

-- | this will return the Worst value so far and marker of a pruning tree
-- sorted snd means I will use the second value to index as this is worst
worstLegalResult :: GameState -> Score
worstLegalResult state = snd(minimize (pruneDepth 5 (resultTree state)))

-- | helper function to find the pruneDepth
pruneDepth :: Int -> Rose a -> Rose a
pruneDepth n (Rose a list)
      | n == 0 = Rose a []
      | otherwise = Rose a (map (pruneDepth (n-1)) list)  

-- | This Function set will cut the tree with the depths or rows that you want
getBestResult :: GameState -> Int -> Move
getBestResult state score = (allLegalMove state) !! score
  where
    score = bestLegalResult state

getWorstResult :: GameState -> Int -> Move
getWorstResult state score  = (allLegalMove state) !! score
  where
    score = worstLegalResult state

-- | this function gives ghe final alpha beta pruning ai move
alphaAi :: Course -> GameState -> Int -> Move
alphaAi _ state n = case turn state of
  Turn Player1 ->  getWorstResult state n
  Turn Player2 -> getBestResult state n
  _ -> error"GameOver!!!"

