module Src.Dirt where 

import Environment ( Environment(dirts, empties, kids) )
import Src.Element ( Element(EmptyCell, Dirt, pos), first, second )
import Core.Utils ( removeItem, pickRandom, manhattanDistance )

isDirt:: Element -> Bool
isDirt element = case element of
    (Dirt (_,_)) -> True 
    _ -> False


inSquare:: Element -> Element -> Bool
inSquare p1 p2
    | abs (first p1 - first p2) >= 2 = False
    | abs (second p1 - second p2) >= 2 = False
    | manhattanDistance (pos p1) (pos p2) <= 2 = True
    | otherwise = False


randomlyPutDirt:: Environment -> [Element] -> Int -> Environment
randomlyPutDirt env _ 0 = env
randomlyPutDirt env emptiesElements maxToPut = let randomEmptyElement = pickRandom emptiesElements 
    in let updatedEnvironment =  env {dirts = removeItem (Dirt (pos randomEmptyElement)) (dirts env) ++ [Dirt (pos randomEmptyElement)] ,empties = removeItem (EmptyCell(pos randomEmptyElement)) (empties env) }
    in randomlyPutDirt updatedEnvironment emptiesElements (maxToPut - 1)

maxPosibleDirt:: Int -> Int -> Int 
maxPosibleDirt nKids nEmpties
    | nKids == 0 = 0
    | nKids == 1 = min 1 nEmpties
    | nKids == 2 = min 3 nEmpties
    | otherwise = min 6 nEmpties

-- Generates dirt in a 3x3 square
generateInSquare:: Environment -> Element -> Environment

generateInSquare environment position =
    let emptiesInSquare = filter (inSquare position) (empties environment)
            in let nKids = length (filter (inSquare position) (kids environment))
                in randomlyPutDirt environment emptiesInSquare (maxPosibleDirt nKids (length emptiesInSquare))
    
