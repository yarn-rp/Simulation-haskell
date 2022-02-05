module BotAgentV2 where

import Environment
    ( Environment(kidsInJail, kids, botsWithKid, empties, dirts, bots),
      getElementAtPosition )
import Element
    ( Element(Corral, BotWithKid, BotAgentV2, EmptyCell, Kid, Dirt,
              pos),
      Position )
import Utils ( removeItem, manhattanDistance, (|>), headSafe )
import Data.List ( sortOn )
import BFS ( bfsSearch )


moveAll:: Environment -> Environment
moveAll environment = 
    foldl BotAgentV2.move environment (bots environment)

sortTuplesDistances (a1, b1) (a2, b2) = compare b1 b2

nearestKid:: Environment -> Position -> Maybe Element
nearestKid env pos = nearestPositionInCollection env pos (kids env)
    
nearestDirt:: Environment -> Position -> Maybe Element
nearestDirt env pos = nearestPositionInCollection env pos (dirts env)

nearestPositionInCollection::Environment -> Position  -> [Element] -> Maybe Element
nearestPositionInCollection env initialPos collection = let elementWithDistances = map (\element -> (element, manhattanDistance initialPos (pos element)) ) collection
    in sortOn snd elementWithDistances
    |> headSafe -- devuelve el kid
    |> fmap fst
  

tryGoToPosition:: Environment -> Element -> Position -> Environment
tryGoToPosition environment bot position = let element = getElementAtPosition environment position in
    let isKidInJailInPosition = any (\a -> a == (pos bot)) (map pos (kidsInJail environment))
    in case element of
        Nothing -> environment
        Just element -> 
            case (element,isKidInJailInPosition) of
                (EmptyCell(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        bots = (removeItem bot (bots environment)) ++ [ BotAgentV2 (pos element)]
                    }
                (EmptyCell(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot)],
                        bots = (removeItem bot (bots environment)) ++ [ BotAgentV2 (pos element)]
                    }
                (Kid(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        kids = removeItem element (kids environment),
                        bots = (removeItem bot (bots environment)),
                        botsWithKid = botsWithKid environment ++ [BotWithKid(pos element)]
                    }
                (Kid(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        kids = removeItem element (kids environment),
                        bots = (removeItem bot (bots environment)),
                        botsWithKid = botsWithKid environment ++ [BotWithKid(pos element)]
                    }
                (Dirt(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        dirts = removeItem element (dirts environment),
                        bots = (removeItem bot (bots environment) ) ++ [ BotAgentV2( pos element ) ]
                    }
                (Dirt(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        dirts = removeItem element (dirts environment),
                        bots = (removeItem bot (bots environment) ) ++ [ BotAgentV2( pos element ) ]
                    }
                (_) -> environment


move:: Environment -> Element -> Environment
move env bot = let (kidToPick , dirtToGo) = (nearestKid env (pos bot), nearestDirt env (pos bot)) in 
    let (bestPathToKid,bestPathToDirt ) = (fmap (shortestPathToElement env (pos bot)) kidToPick, fmap (shortestPathToElement env (pos bot)) dirtToGo)
    in case (bestPathToKid , bestPathToDirt) of
        (Just [],Just []) -> env
        (Just [], Just value) -> tryGoToPosition env bot (head value)
        (Nothing , Nothing) -> env
        (Nothing , Just []) -> env
        (Nothing , Just value) -> tryGoToPosition env bot (head value)
        (Just [] , _) -> env
        (Just value , _) -> tryGoToPosition env bot (head value)


-------------------------------------------------Smart stuff------------------------------------------------------------

--Similar to isDirty, but returns an int
isDirtInt:: Maybe Element -> Int
isDirtInt element = case element of 
            Nothing -> 0
            Just element ->
                case (element) of
                    (Dirt (_,_)) -> 1 
                    _ -> 0

-- Gets the dirtiness(amount of dirty elements) of a given path
dirtyness:: Environment -> [Position] -> Int
-- dirtyness env path = sum (map (isDirtInt (.) getElementAtPosition) path)
--TODO: check if this works, porke ta mas chula
dirtyness env path = sum (map (\a -> isDirtInt (getElementAtPosition env a)) path)

shortestPathToElement:: Environment -> Position -> Element -> [Position]
shortestPathToElement env position element= bfsSearch env position isWalkable (pos element) 

isWalkable:: Element-> Bool 
isWalkable element = case (element) of
    (EmptyCell(_,_)) -> True
    (Kid(_,_)) -> True
    (Dirt(_,_)) -> True
    (Corral(_,_)) -> True
    (_) -> False
    