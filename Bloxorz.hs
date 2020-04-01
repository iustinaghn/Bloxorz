{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)

{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = HardCell | SoftCell | Block | Switch | WinCell| EmptySpace
    deriving (Eq, Ord)

instance Show Cell where
    show HardCell = [hardTile]
    show SoftCell = [softTile]
    show Block = [block]
    show Switch = [switch]
    show WinCell = [winningTile]
    show EmptySpace = [emptySpace]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}


data Level = L
  { arr :: A.Array Position Cell
  , position1 :: Position
  , position2 :: Position
  , positions :: [Position]
  } deriving (Eq, Ord)



{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

winOrLose :: Level -> String
winOrLose (L arr position1 position2 positions) = if (arr A.! position1) == EmptySpace || (arr A.! position2) == EmptySpace
                                                    then "Game Over\n"
                                                        else if (arr A.! position1) == SoftCell && (arr A.! position2) == SoftCell
                                                            then "Game Over\n"
                                                                else if (arr A.! position1) == WinCell && (arr A.! position2) == WinCell
                                                                    then "Congrats! You won!\n"
                                                                        else ""

instance Show Level where
   show (L array pos1 pos2 posi) = "\n" ++ (unlines [concat [if (y,x) == pos1 || (y,x) == pos2
                                                            then [block] 
                                                                else show (array A.! (y, x)) | x <- [0..(snd (snd (A.bounds array)))]] 
                                                                                             | y <- [0..(fst (snd (A.bounds array)))]]) 
                                    ++ (winOrLose (L array pos1 pos2 posi))



{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel coord pos = (L array pos pos [])
    where array = A.array ((0,0), coord) [if (x,y) /= pos 
                                            then ((x,y), EmptySpace) 
                                                else ((x,y), Block) | x <- [0..(fst coord)],
                                                                      y <- [0..(snd coord)]]

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}

addTile :: Char -> Position -> Level -> Level
addTile ch pos (L array position1 position2 positions) = (L arr position1 position2 positions)
    where arr = array A.// [if ch == 'H' 
                                then (pos, HardCell) 
                                    else if ch == 'S' 
                                        then (pos, SoftCell) 
                                            else if ch == 'W' 
                                                then (pos, WinCell) 
                                                    else (pos, EmptySpace)]
    


{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}


addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos listPos (L array position1 position2 positions) = (L arr position1 position2 posi)
    where arr = array A.// [(pos, Switch)]
          posi = listPos



{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}


activate :: Cell -> Level -> Level
activate cell (L array position1 position2 positions) = (L arr position1 position2 positions)
    where arr = array A.// [if (array A.! p) == EmptySpace && ((array A.! position1 == cell) || (array A.! position2 == cell))
                                then (p, HardCell)
                                    else if (array A.! p) == HardCell && ((array A.! position1 == cell) || (array A.! position2 == cell))
                                        then (p, EmptySpace)
                                            else (p, (array A.! p)) | p <- positions]

                        

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}
moveNorth :: Level -> Level
moveNorth (L array position1 position2 positions) = if position1 == position2 
                                                then (L array ((fst position1) - 2, (snd position1)) ((fst position2) - 1, (snd position2)) positions) 
                                                    else if ((fst position2) == (fst position1) + 1 && (snd position1) == (snd position2)) 
                                                        then (L array ((fst position1) - 1, (snd position1)) ((fst position2) - 2, (snd position2)) positions) 
                                                            else (L array ((fst position1) - 1, (snd position1)) ((fst position2) - 1, (snd position2)) positions)

moveSouth :: Level -> Level
moveSouth (L array position1 position2 positions) = if position1 == position2 
                                            then (L array ((fst position1) + 1, (snd position1)) ((fst position2) + 2, (snd position2)) positions) 
                                                else if ((fst position2) == (fst position1) + 1 && (snd position1) == (snd position2)) 
                                                    then (L array ((fst position1) + 2, (snd position1)) ((fst position2) + 1, (snd position2)) positions) 
                                                        else (L array ((fst position1) + 1, (snd position1)) ((fst position2) + 1, (snd position2)) positions)

moveWest :: Level -> Level
moveWest (L array position1 position2 positions) = if position1 == position2 
                                            then (L array ((fst position1), (snd position1) - 2) ((fst position2), (snd position2) - 1) positions) 
                                                else if ((fst position2) == (fst position1) + 1 && (snd position1) == (snd position2)) 
                                                    then (L array ((fst position1), (snd position1) - 1) ((fst position2), (snd position2) - 1) positions) 
                                                        else (L array ((fst position1), (snd position1) - 1) ((fst position2), (snd position2) - 2) positions)

moveEast :: Level -> Level
moveEast (L array position1 position2 positions) = if position1 == position2 
                                            then (L array ((fst position1), (snd position1) + 1) ((fst position2), (snd position2) + 2) positions) 
                                                else if ((fst position2) == (fst position1) + 1 && (snd position1) == (snd position2)) 
                                                    then (L array ((fst position1), (snd position1) + 1) ((fst position2), (snd position2) + 1) positions) 
                                                        else (L array ((fst position1), (snd position1) + 1) ((fst position2), (snd position2) + 2) positions)



move :: Directions -> Level -> Level
move dir (L array position1 position2 positions) = if dir == North 
                                            then (activate Switch (moveNorth (L array position1 position2 positions)))
                                                else if dir == South
                                                    then (activate Switch (moveSouth (L array position1 position2 positions)))
                                                        else if dir == East
                                                            then (activate Switch (moveEast (L array position1 position2 positions)))
                                                                else (activate Switch (moveWest (L array position1 position2 positions)))
    

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (L array position1 position2 positions) = if (winOrLose (L array position1 position2 positions)) /= "Game Over\n" 
                                                        && (winOrLose (L array position1 position2 positions)) /= "Congrats! You won!\n"
                                                            then True
                                                                else False

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors (L array position1 position2 positions) = pair
        where pair = [(dir, (move dir (L array position1 position2 positions))) | dir <- [North, South, West, East], (continueGame (move dir (L array position1 position2 positions))) == True]
        
    isGoal (L arr position1 position2 positions) = if (arr A.! position1) == EmptySpace 
                                                    then False
                                                        else if (arr A.! position1) == SoftCell && (arr A.! position2) == SoftCell
                                                            then False
                                                                else if (arr A.! position1) == WinCell && (arr A.! position2) == WinCell
                                                                    then True
                                                                        else if (arr A.! position1) == EmptySpace && (arr A.! position2) == EmptySpace
                                                                            then False
                                                                                else False
    -- Doar petru BONUS
    -- heuristic = undefined
