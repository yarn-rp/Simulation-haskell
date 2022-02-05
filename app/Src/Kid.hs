module Src.Kid (walkAll) where 
import Environment
    ( Environment(empties, obstacles, kids), getElementAtPosition )
import Src.Cell
    ( Cell(Obstacle, Kid, EmptyCell, pos),
      Position,
      isNear,
      directionToMove )
import Core.Utils ( removeItem, pickRandom )
import Src.Obstacle 
import Src.Dirt 


tryGoToPosition:: Environment -> Cell -> Position -> Environment
tryGoToPosition environment kid position = let element = getElementAtPosition environment position in
    case element of 
        Nothing -> environment
        Just element -> case (kid,environment,element) of
            (_,_,EmptyCell(_,_)) -> let envStillClean =  environment {
                empties = removeItem element (empties environment) ++ [ EmptyCell (pos kid)],
                kids = removeItem kid (kids environment) ++ [ Kid (pos element)]
                } in Src.Dirt.gen envStillClean element
            (_,_,_) -> environment


validCell:: Cell -> Cell-> Bool
validCell = isNear


getPosiblePositions:: Environment -> Cell -> [Cell] 
getPosiblePositions environment kid = filter ( validCell kid ) (empties environment ++ obstacles environment)

getRandomPosition:: Environment-> Cell -> Cell 
getRandomPosition environment kid
    | not (null (getPosiblePositions environment kid)) = pickRandom (getPosiblePositions environment kid)
    | otherwise = kid


deleteKid:: Environment -> Cell -> Environment 
deleteKid environment kid = 
        environment {kids = removeItem kid (kids environment)}

walk:: Environment -> Cell -> Environment
walk environment kid = 
        let element = getRandomPosition environment kid
            in case element of
                (EmptyCell (_,_)) -> Src.Kid.tryGoToPosition environment kid (pos element)
                (Obstacle (n,m)) -> let direction = directionToMove kid (n,m)
                                    in let envWithObstaclesMoved = Src.Obstacle.walk environment (Obstacle(n,m)) direction 
                                    in Src.Kid.tryGoToPosition envWithObstaclesMoved  kid (pos element)
                _ -> environment


walkAll:: Environment -> Environment
walkAll environment = 
    foldl Src.Kid.walk environment (kids environment)