module Src.Dirt where 

import Environment ( Environment(dirts, empties, kids) )
import Src.Cell ( Cell(EmptyCell, Dirt, pos), first, second )
import Core.Utils ( removeItem, pickRandom, manhattanDistance )


gen:: Environment -> Cell -> Environment
gen environment position =
    let emptiesInSquare = filter (isSurroundedLen3 position) (empties environment)
            in let nKids = length (filter (isSurroundedLen3 position) (kids environment))
                in randomlyPutDirt environment emptiesInSquare (maxPosibleDirt nKids (length emptiesInSquare))

isSurroundedLen3:: Cell -> Cell -> Bool
isSurroundedLen3 p1 p2
    | abs (first p1 - first p2) >= 2 = False
    | abs (second p1 - second p2) >= 2 = False
    | manhattanDistance (pos p1) (pos p2) <= 2 = True
    | otherwise = False

maxPosibleDirt:: Int -> Int -> Int 
maxPosibleDirt nKids nEmpties
    | nKids == 0 = 0
    | nKids == 1 = min 1 nEmpties
    | nKids == 2 = min 3 nEmpties
    | otherwise = min 6 nEmpties    

randomlyPutDirt:: Environment -> [Cell] -> Int -> Environment
randomlyPutDirt env _ 0 = env
randomlyPutDirt env emptiesElements maxToPut = let randomEmptyElement = pickRandom emptiesElements 
    in let updatedEnvironment =  env {dirts = removeItem (Dirt (pos randomEmptyElement)) (dirts env) ++ [Dirt (pos randomEmptyElement)] ,empties = removeItem (EmptyCell(pos randomEmptyElement)) (empties env) }
    in randomlyPutDirt updatedEnvironment emptiesElements (maxToPut - 1)