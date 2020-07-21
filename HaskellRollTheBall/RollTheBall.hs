{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A
import Data.Maybe

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell {draw :: Char} --TODO
    deriving (Eq, Ord)

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level (A.Array (Int, Int) Cell)--TODO
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

instance Show Cell
    where show cell = [draw cell]

instance Show Level 
    where show (Level arr) = let bnd = snd (A.bounds arr)
                            in  endl : concat [printline x arr | x <-[0..(fst bnd)]]

{-  helper function that receives a line index and an array and returns a string created
    from that line with an endline -}
printline :: Int -> Array (Int, Int) Cell -> String
printline line arr =  let b = snd (A.bounds arr)
                in [draw (arr A.! (line, y)) | y <- [0..(snd b)]] ++ [endl]
{-
    receives down-right limits and creates an emptyLevel by iterating
    through the array and adding emptySpaces everywhere
-}

emptyLevel :: Position -> Level
emptyLevel (x, y) = Level $ A.array ((0,0), (x, y)) 
                    [((i, j), (Cell emptySpace)) | i <- [0..x], j <- [0..y]]

{- 
    modifies a cell adding something else on top of an emptySpace
    after checking if indices are correct
-}
addCell :: (Char, Position) -> Level -> Level
addCell (tip, (x, y)) (Level arr) = Level $ if (validPos x y arr) && draw (arr A.! (x, y)) == emptySpace
                                                then arr A.// [((x, y), (Cell tip))] else arr

-- checks if a position is inside the board
validPos :: Int -> Int -> Array (Int, Int) Cell -> Bool
validPos x y arr = let bnd = snd (A.bounds arr) 
                    in x >= 0 && x <= (fst bnd) && y >= 0 && y <= (snd bnd)

-- uses a foldr to add all cells from given list over an emptyLevel
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos = foldr addCell (emptyLevel pos)


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}
{-
    first checks if destination is an emptySpace, then it adds our new cell
    on top of that emptySpace and then update initial position by putting
    an emptySpace there
-}
moveCell :: Position -> Directions -> Level -> Level
moveCell (x, y) dir (Level arr) = let dstCheck = canMove x y dir arr
                                      val = draw $ arr A.! (x, y)
                                      srcCheck = validPos x y arr
                                    in if dstCheck == Nothing || not srcCheck then Level arr 
                                        else removeCell (x, y) $ addCell (val, fromJust dstCheck) (Level arr)

-- replaces a cell at a given position with an emptySpace
removeCell :: Position -> Level -> Level
removeCell pos (Level arr) = Level $ arr A.// [(pos, (Cell emptySpace))]

{- first check if we try to move a start or win cell
    then we take every direction and check if destination isn't out of bounds and if destination is emptySpace
-}

canMove :: Int -> Int -> Directions -> Array (Int, Int) Cell -> Maybe Position
canMove x y dir arr
        | elem (draw (arr A.! (x, y))) (startCells ++ winningCells) = Nothing
        | (draw (arr A.! (x, y))) == emptySpace = Nothing
        | dir == East && validPos x (y + 1) arr = if draw (arr A.! (x, y + 1))
                                            == emptySpace then Just (x, y + 1) else Nothing
        | dir == West && validPos x (y - 1) arr = if draw (arr A.! (x, y - 1))
                                            == emptySpace then Just (x, y - 1) else Nothing
        | dir == North && validPos (x - 1) y arr = if draw (arr A.! (x - 1, y))
                                            == emptySpace then Just (x - 1, y) else Nothing
        | dir == South && validPos (x + 1) y arr = if draw (arr A.! (x + 1, y))
                                            == emptySpace then Just (x + 1, y) else Nothing
        | otherwise = Nothing

{-
    helper lists with all pipe types grouped by possible connection direction
-}
eastCon :: [Char]
eastCon = [horPipe, topLeft, botLeft, startRight, winRight]
westCon :: [Char]
westCon = [horPipe, topRight, botRight, startLeft, winLeft]
northCon :: [Char]
northCon = [verPipe, botRight, botLeft, startUp, winUp]
southCon :: [Char]
southCon = [verPipe, topRight, topLeft, startDown, winDown]

-- helper function that checks if two cells can connect in a given direction
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell c1) (Cell c2) dir
                | dir == East = (elem c1 eastCon) && (elem c2 westCon)
                | dir == West = (elem c1 westCon) && (elem c2 eastCon)
                | dir == North = (elem c1 northCon) && (elem c2 southCon)
                | dir == South = (elem c1 southCon) && (elem c2 northCon)
                | otherwise = False


-- call find start and then start checking the path
wonLevel :: Level -> Bool
wonLevel (Level arr) = let start = findStart (Level arr)
                    in checkPath (Level arr) start

-- recursive function that checks the path by trying to find connections in every direction
-- also it replaces the cell we leave so it won't cycle
checkPath :: Level -> Position -> Bool
checkPath (Level arr) (x, y)
        | elem (draw (arr A.! (x, y))) winningCells = True
        | validPos x (y + 1) arr && connection (arr A.! (x, y)) (arr A.! (x, y + 1))
                                        East = checkPath (removeCell (x, y) (Level arr)) (x, y + 1)
        | validPos x (y - 1) arr && connection (arr A.! (x, y)) (arr A.! (x, y - 1))
                                        West = checkPath (removeCell (x, y) (Level arr)) (x, y - 1)
        | validPos (x + 1) y arr && connection (arr A.! (x, y)) (arr A.! (x + 1, y))
                                        South = checkPath (removeCell (x, y) (Level arr)) (x + 1, y)
        | validPos (x - 1) y arr && connection (arr A.! (x, y)) (arr A.! (x - 1, y))
                                        North = checkPath (removeCell (x, y) (Level arr)) (x - 1, y)
        | otherwise = False

 -- helper function that gets indices from level array and filters them to return only startCell's position                          
findStart :: Level -> Position
findStart (Level arr) = head $ filter (\x -> elem (draw (arr A.! x)) startCells) $ A.indices arr

 {- helper that finds all successors from a state looking to make moves in only one direction;
    it uses a foldr so we sort out the impossible moves using the canMove function
 -}
findSucc :: Directions -> Level -> [((Position, Directions), Level)]
findSucc dir lvl@(Level arr) = foldr filterSucc [] (A.assocs arr)
        where filterSucc ((x, y), _) acc = if isNothing (canMove x y dir arr) 
                                            then acc else (((x, y), dir), moveCell (x, y) dir lvl) : acc
instance ProblemState Level (Position, Directions) where
    {- 
    we first apply findStart in all directions than filter out the states that didn't change the
    level (default case in moveCell)
    -}
    successors lvl = findSucc East lvl ++ findSucc West lvl
                                       ++ findSucc North lvl
                                       ++ findSucc South lvl

    isGoal = wonLevel
    reverseAction state@(((x, y), dir), lvl)
                    | dir == East = (((x, y + 1), West), moveCell (x, y + 1) West lvl)
                    | dir == West = (((x, y - 1), East), moveCell (x, y - 1) East lvl)
                    | dir == North = (((x - 1, y), South), moveCell (x - 1, y) South lvl)
                    | dir == South = (((x + 1, y), North), moveCell (x + 1, y) North lvl)
                    | otherwise = state
