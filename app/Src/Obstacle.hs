module Src.Obstacle (walk)  where 

import Environment
    ( Environment(empties, obstacles), getElementAtPosition )
import Src.Cell
    ( Direction, Cell(Obstacle, EmptyCell, pos), Position, add )
import Core.Utils ( removeItem )

tryGoToPosition:: Environment -> Cell -> Position -> Environment
tryGoToPosition environment obstacle position = let element = getElementAtPosition environment position in
    case element of 
        Nothing -> environment
        Just element -> 
            case (obstacle,environment,element) of
                (_,_,EmptyCell(_,_)) -> environment {
                    empties = removeItem element (empties environment) ++ [ EmptyCell (pos obstacle)],
                    obstacles = removeItem obstacle (obstacles environment) ++ [ Obstacle (pos element)]
                    }
                (_,_,_) -> environment

shiftIfObstacle:: Environment -> Position -> Direction -> Environment
shiftIfObstacle environment position direction = 
    let element = getElementAtPosition environment position in
        case element of 
            Nothing -> environment
            Just element -> 
                case (environment ,element, direction) of
                    (environment ,Obstacle(n,m), direction) ->  walk environment (Obstacle(n,m)) direction
                    (environment, _ , _) -> environment

walk:: Environment -> Cell -> Direction -> Environment
walk environment obstacle direction =
    let position = add obstacle direction
            in tryGoToPosition (shiftIfObstacle environment position direction) obstacle position

