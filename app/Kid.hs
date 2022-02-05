module Kid (moveAll) where 
import Environment
    ( Environment(empties, obstacles, kids), getElementAtPosition )
import Element
    ( Element(Obstacle, Kid, EmptyCell, pos),
      Position,
      isNear,
      directionToMove )
import Utils ( removeItem, pickRandom )
import Obstacle ( move )
import Dirt ( generateInSquare )


tryGoToPosition:: Environment -> Element -> Position -> Environment
tryGoToPosition environment kid position = let element = getElementAtPosition environment position in
    case element of 
        Nothing -> environment
        Just element -> case (kid,environment,element) of
            (_,_,EmptyCell(_,_)) -> let envStillClean =  environment {
                empties = removeItem element (empties environment) ++ [ EmptyCell (pos kid)],
                kids = removeItem kid (kids environment) ++ [ Kid (pos element)]
                } in Dirt.generateInSquare envStillClean element
            (_,_,_) -> environment


isValidMovement:: Element ->Element-> Bool
isValidMovement = isNear


getPosiblePositions:: Environment -> Element -> [Element] 
getPosiblePositions environment kid = filter ( isValidMovement kid ) (empties environment ++ obstacles environment)

-- TODO: concat posible positions with kid, in this implementation the kid choose `no move` only when he has no move
getRandomPosition:: Environment-> Element -> Element 
getRandomPosition environment kid
    | not (null (getPosiblePositions environment kid)) = pickRandom (getPosiblePositions environment kid)
    | otherwise = kid


deleteKid:: Environment -> Element -> Environment 
deleteKid environment kid = 
        environment {kids = removeItem kid (kids environment)}

move:: Environment -> Element -> Environment
move environment kid = 
        let element = getRandomPosition environment kid
            in case element of
                (EmptyCell (_,_)) -> Kid.tryGoToPosition environment kid (pos element)
                (Obstacle (n,m)) -> let direction = directionToMove kid (n,m)
                                    in let envWithObstaclesMoved = Obstacle.move environment (Obstacle(n,m)) direction 
                                    in Kid.tryGoToPosition envWithObstaclesMoved  kid (pos element)
                _ -> environment


moveAll:: Environment -> Environment
moveAll environment = 
    foldl Kid.move environment (kids environment)