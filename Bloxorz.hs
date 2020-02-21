{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import Data.Matrix
import qualified Data.Vector as V

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

data Cell = Cell
  { tilePos :: Position
  , tileType :: Char
  , activePos :: [Position]
  } deriving (Eq, Ord)

instance Show Cell where
    show (Cell _ tile _) = [tile]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level
  { gameMap :: Matrix Cell
  , state :: [Char]
  , blockPosition :: [Position]
  , winningPosition :: Position
  }

{-
    *** Opțional ***

    Dacă aveți nevoie de o funcționalitate particulară,
    instantiati explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

instance Eq Level where
   (==) (Level gM1 _ bP1 _) (Level gM2 _ bP2 _) = (bP1 == bP2 || bP1 == reverse bP2) && gM1 == gM2

instance Ord Level where
   compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show.

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou.
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n".
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n".
-}

showHelper :: Level -> [Char] -> Cell -> [Char]
showHelper (Level _ _ blockPos _ ) acc currentTile@(Cell pos _ _ )
    | elem pos blockPos = acc ++ show (Cell pos block [])
    | otherwise = acc ++ show currentTile

instance Show Level where
    show currentLevel@(Level gM st _ _) = (foldl (++) "" mapLines) ++ "\n" ++ finalState
          where
              mapLines = [ V.foldl (showHelper currentLevel) "\n" (getRow y gM) | y <- [1 .. (nrows gM)]]
              finalState
                  | st == "Win" = "Congrats! You won!\n"
                  | st == "Loss" = "Game Over\n"
                  | otherwise = ""

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel (x1, y1) (x2, y2) = Level gM "Playing" [(x2 + 1, y2 + 1)] (x1 + 2, y1 + 2)
    where
        gM = matrix (x1 + 1) (y1 + 1) (\ (i, j) -> (Cell (i, j) emptySpace []))

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat:
        'H' pentru tile hard
        'S' pentru tile soft
        'W' pentru winning tile
-}

addTile :: Char -> Position -> Level -> Level
addTile tile (x, y) (Level gM st blockPos winningPos) = Level newGameMap st blockPos win
    where
        newGameMap = setElem (Cell (x + 1, y + 1) gameTile []) (x + 1, y + 1) gM
        gameTile
            | tile == 'H' = hardTile
            | tile == 'S' = softTile
            | otherwise = winningTile

        win
            | tile == 'W' = (x + 1, y + 1)
            | otherwise = winningPos

{-
    *** TODO ***

    Adaugă o celulă de tip Switch în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch (x, y) positions (Level gM st blockPos winningPos) = Level newGameMap st blockPos winningPos
    where
        newGameMap = setElem (Cell (x + 1, y + 1) switch positions) (x + 1, y + 1) gM

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică.
    În funcție de mecanica activată, vor avea loc modificări pe hartă.
-}

transform :: Level -> Position -> Level
transform (Level gM st blockPos winningPos) (x, y) = Level newGameMap st blockPos winningPos
    where
        newGameMap = setElem (Cell (x + 1, y + 1) newTile []) (x + 1, y + 1) gM
        currentTile = getElem (x + 1) (y + 1) gM
        newTile
            | tileType currentTile == emptySpace = hardTile
            | otherwise = emptySpace

activate :: Cell -> Level -> Level
activate switchCell lvl = foldl transform lvl $ activePos switchCell

{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}

onMap :: [Position] -> Matrix Cell -> Bool
onMap positions gM = null $ filter check positions
    where
        check = (\ (x, y) -> x < 1 || x > nrows gM || y < 1 || y > ncols gM || (tileType $ getElem x y gM) == emptySpace)

setActive :: Level -> Position -> Level
setActive level@(Level gM _ _ _ ) (x, y) = activate (getElem x y gM) level

move :: Directions -> Level -> Level
move _ currentLevel@(Level _ "Win" _ _ ) = currentLevel
move _ currentLevel@(Level _ "Loss" _ _ ) = currentLevel
move direction (Level gM st [(x, y)] win) = if onMap newBlockPos gM
                                            then foldl setActive newLevel switchBlock
                                            else newLevel
      where
          newBlockPos
              | direction == North = [(x - 2, y), (x - 1, y)]
              | direction == South = [(x + 1, y), (x + 2, y)]
              | direction == West = [(x, y - 2), (x, y - 1)]
              | direction == East = [(x, y + 1), (x, y + 2)]
              | otherwise = []

          switchBlock = filter (\ (i, j) -> (tileType $ getElem i j gM) == switch) newBlockPos
          wonLevel = Level gM "Win" newBlockPos win
          lostLevel = Level gM "Loss" newBlockPos win
          newLevel
              | not $ onMap newBlockPos gM = lostLevel
              | length newBlockPos == 1 &&
                    (tileType $ getElem (fst $ head newBlockPos) (snd $ head newBlockPos) gM) == winningTile = wonLevel
              | length newBlockPos == 1 &&
                    (tileType $ getElem (fst $ head newBlockPos) (snd $ head newBlockPos) gM) == softTile = lostLevel
              | otherwise = (Level gM st newBlockPos win)

move direction (Level gM st [(x, y), (u, v)] win) = if onMap newBlockPos gM
                                                    then foldl setActive newLevel switchBlock
                                                    else newLevel
      where
          newBlockPos
              | x == u && direction == North = [(x - 1, y), (u - 1, v)]
              | x == u && direction == South = [(x + 1, y), (u + 1, v)]
              | x == u && direction == West = [(x, y - 1)]
              | x == u && direction == East = [(u, v + 1)]
              | y == v && direction == North = [(x - 1, y)]
              | y == v && direction == South = [(u + 1, v)]
              | y == v && direction == West = [(x, y - 1), (u, v - 1)]
              | y == v && direction == East = [(x, y + 1), (u, v + 1)]
              | otherwise = []

          switchBlock = filter (\ (i, j) -> (tileType $ getElem i j gM) == switch) newBlockPos
          wonLevel = Level gM "Win" newBlockPos win
          lostLevel = Level gM "Loss" newBlockPos win
          newLevel
              | not $ onMap newBlockPos gM = lostLevel
              | length newBlockPos == 1 &&
                    (tileType $ getElem (fst $ head newBlockPos) (snd $ head newBlockPos) gM) == winningTile = wonLevel
              | length newBlockPos == 1 &&
                    (tileType $ getElem (fst $ head newBlockPos) (snd $ head newBlockPos) gM) == softTile = lostLevel
              | otherwise = (Level gM st newBlockPos win)

move _ _ = undefined

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level _ "Win" _ _ ) = False
continueGame (Level _ "Loss" _ _ ) = False
continueGame _ = True

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.

    Hint: Un level câștigat nu are succesori!
    De asemenea, puteți ignora succesorii care
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors (Level _ "Win" _ _ ) = []
    successors (Level _ "Loss" _ _ ) = []
    successors level = [(direction, move direction level) | direction <- [North, South, West, East],
                                                            (state $ move direction level) /= "Loss"]

    isGoal (Level _ "Win" _ _ ) = True
    isGoal _ = False

    -- Doar petru BONUS
    heuristic (Level _ _ [(x, _)] _ ) = x
    heuristic (Level _ _ [(x, _), (u, _)] _ ) = min x u
    heuristic _ = undefined
