module Src.Bot  where 
import Environment
    ( Environment(kidsInCorral, kids, botsWithKid, empties, dirts, bots),
      getElementAtPosition )
import Src.Cell
    ( Cell(Corral, BotWithKid, Bot, EmptyCell, Kid, Dirt, pos),
      Position )
import Core.Utils ( removeItem, manhattanDistance, (|>), headSafe )
import Data.List ( sortOn )
import Core.Bfs ( bfsSearch )


walkAll:: Environment -> Environment
walkAll environment = 
    foldl Src.Bot.walk environment (bots environment)

sortTuplesDistances :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
sortTuplesDistances (a1, b1) (a2, b2) = compare b1 b2

nearestKid:: Environment -> Position -> Maybe Cell
nearestKid env pos = nearestPositionInCollection env pos (kids env)
    
nearestDirt:: Environment -> Position -> Maybe Cell
nearestDirt env pos = nearestPositionInCollection env pos (dirts env)

nearestPositionInCollection::Environment -> Position  -> [Cell] -> Maybe Cell
nearestPositionInCollection env initialPos collection = let elementWithDistances = map (\element -> (element, manhattanDistance initialPos (pos element)) ) collection
    in sortOn snd elementWithDistances
    |> headSafe
    |> fmap fst
  

tryGoToPosition:: Environment -> Cell -> Position -> Environment
tryGoToPosition environment bot position = let element = getElementAtPosition environment position in
    let isKidInCorralInPosition = any ((\ a -> a == pos bot) . pos) (kidsInCorral environment)
    in case element of
        Nothing -> environment
        Just element -> 
            case (element,isKidInCorralInPosition) of
                (EmptyCell(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        bots = removeItem bot (bots environment) ++ [ Bot (pos element)]
                    }
                (EmptyCell(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot)],
                        bots = removeItem bot (bots environment) ++ [ Bot (pos element)]
                    }
                (Kid(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        kids = removeItem element (kids environment),
                        bots = removeItem bot (bots environment),
                        botsWithKid = botsWithKid environment ++ [BotWithKid(pos element)]
                    }
                (Kid(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        kids = removeItem element (kids environment),
                        bots = removeItem bot (bots environment),
                        botsWithKid = botsWithKid environment ++ [BotWithKid(pos element)]
                    }
                (Dirt(_,_),True) -> environment {
                        empties = removeItem element (empties environment),
                        dirts = removeItem element (dirts environment),
                        bots = removeItem bot (bots environment)  ++ [ Bot( pos element ) ]
                    }
                (Dirt(_,_),False) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        dirts = removeItem element (dirts environment),
                        bots = removeItem bot (bots environment)  ++ [ Bot( pos element ) ]
                    }
                _ -> environment


walk:: Environment -> Cell -> Environment
walk env bot = let (kidToPick , dirtToGo) = (nearestKid env (pos bot), nearestDirt env (pos bot)) in 
    let (bestPathToKid,bestPathToDirt ) = (fmap (shortestPathToElement env (pos bot)) kidToPick, fmap (shortestPathToElement env (pos bot)) dirtToGo)
    in case (bestPathToKid , bestPathToDirt) of
        (Just [],Just []) -> env
        (Just [], Just value) -> tryGoToPosition env bot (head value)
        (Nothing , Nothing) -> env
        (Nothing , Just []) -> env
        (Nothing , Just value) -> tryGoToPosition env bot (head value)
        (Just [] , _) -> env
        (Just value , _) -> tryGoToPosition env bot (head value)


shortestPathToElement:: Environment -> Position -> Cell -> [Position]
shortestPathToElement env position element= bfsSearch env position isWalkable (pos element) 

isWalkable:: Cell-> Bool 
isWalkable element = case element of
    (EmptyCell(_,_)) -> True
    (Kid(_,_)) -> True
    (Dirt(_,_)) -> True
    (Corral(_,_)) -> True
    _ -> False
    