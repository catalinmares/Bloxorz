{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe
import Data.List

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node
 { state :: s
 , lastDir :: Maybe a
 , parent :: Maybe (Node s a)
 , depth :: Int
 , children :: [Node s a]
 }

instance (Eq s) => Eq (Node s a) where
    (==) n1 n2 = state n1 == state n2

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState node = state node

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

createHelper :: (Eq s, Eq a) => (ProblemState s a) => Maybe (Node s a) -> Int -> (Maybe a, s) -> Node s a
createHelper predecessor dpt (dir, st) = treeNode
    where
        filtredChildren
            | predecessor == Nothing = successors st
            | otherwise = filter (\ (_, childState) -> nodeState (fromJust predecessor) /= childState) $ successors st

        nodeChildren = map (\ (act, childState) -> createHelper (Just treeNode) (dpt + 1) (Just act, childState)) filtredChildren
        treeNode = Node st dir predecessor dpt nodeChildren

createStateSpace :: (Eq s, Eq a) => (ProblemState s a) => s -> Node s a
createStateSpace st = createHelper Nothing 0 (Nothing, st)

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState.
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
orderStateSpace (Node st dir predecessor dpt succs) = Node st dir predecessor dpt (map (\ n -> orderStateSpace n) orderedChildren)
    where
        orderedChildren = sortBy (\ x y -> compare (heuristic $ state x) (heuristic $ state y)) succs

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}

dfsHelper :: (ProblemState s a, Eq s) => [Node s a] -> Int -> [Node s a] -> [Node s a]
dfsHelper [] _ nodeList = nodeList
dfsHelper (x:xs) dpt nodeList
    | elem x nodeList || depth x > dpt = dfsHelper xs dpt nodeList
    | depth x == dpt = dfsHelper xs dpt (x:nodeList)
    | otherwise = dfsHelper ((children x) ++ xs) dpt (x:nodeList)

limitedDfs :: (ProblemState s a, Eq s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs node dpt = reverse $ dfsHelper [node] dpt []

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Eq s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
iterativeDeepening node = (finalNode, count)
      where
          dfsLists = scanl (\ acc dpt -> acc ++ (limitedDfs node dpt)) [] [0 ..]
          parsedLists = takeWhile (\ lst -> null $ filter (\ x -> isGoal $ state x) lst) dfsLists
          lenLastList = length (last parsedLists)
          lastDpt = length parsedLists - 1
          finalList = limitedDfs node lastDpt
          (lst1, lst2) = break (\ n -> isGoal $ state n) finalList
          finalNode = head lst2
          count = lenLastList + (length lst1)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractNodes :: Node s a -> [Node s a]
extractNodes node = foldl (\ acc _ -> (fromJust $ parent $ head acc) : acc) [node] $ take (depth node - 1) $ repeat 'A'

extractPath :: Node s a -> [(a, s)]
extractPath node = foldl (\ acc x -> (fromJust $ lastDir x, state x) : acc) [] $ reverse $ extractNodes node

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru
    a găsi prima stare finală și reface calea către nodul inițial folosind
    extractPath.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- Starea inițială de la care se pornește
      -> Bool       -- Dacă să folosească sau nu euristica dată
      -> [(a, s)]   -- Lista perechilor
solve st False = extractPath $ fst $ iterativeDeepening $ createStateSpace st
solve st True = extractPath $ fst $ iterativeDeepening $ orderStateSpace $ createStateSpace st

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))
