module Src.BotWithKid where 
import Environment
    ( Environment(dirts, empties, jails, botsWithKid, bots,
                  kidsInCorral),
      getElementAtPosition )
import Src.Cell
    ( Cell( Corral, BotWithKid, Bot, KidInCorral, EmptyCell, Dirt,
              pos),
      Position )
import Core.Utils ( removeItem, manhattanDistance, (|>), headSafe )
import Data.List ( sortOn )
import Core.Bfs ( bfsSearch )


walkAll:: Environment -> Environment
walkAll environment = 
    foldl Src.BotWithKid.walk environment (botsWithKid environment)

sortTuplesDistances :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
sortTuplesDistances (a1, b1) (a2, b2) = compare b1 b2

nearestCorral:: Environment -> Position -> Maybe Cell
nearestCorral env pos = nearestPositionInCollection env pos (jails env)
    
nearestDirt:: Environment -> Position -> Maybe Cell
nearestDirt env pos = nearestPositionInCollection env pos (dirts env)

nearestPositionInCollection::Environment -> Position  -> [Cell] -> Maybe Cell
nearestPositionInCollection env initialPos collection = let elementWithDistances = map (\element -> (element, manhattanDistance initialPos (pos element)) ) collection
    in sortOn snd elementWithDistances
    |> headSafe 
    |> fmap fst
  

tryGoToPosition:: Environment -> Cell -> Position -> Environment
tryGoToPosition environment bot position = let element = getElementAtPosition environment position in
    case element of
        Nothing -> environment
        Just element ->  
            case element of
                (EmptyCell(_,_)) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot)],
                        botsWithKid = removeItem bot (botsWithKid environment) ++ [ BotWithKid (pos element)]
                    }
                (Dirt(_,_)) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        dirts = removeItem element (dirts environment),
                        botsWithKid = removeItem bot (botsWithKid environment) ++ [ BotWithKid (pos element)]
                    }
                (Corral(_,_)) -> environment {
                        empties = removeItem element (empties environment) ++ [ EmptyCell (pos bot) ],
                        jails = removeItem element (jails environment),
                        botsWithKid = removeItem bot (botsWithKid environment),
                        bots = bots environment ++ [ Bot( pos element ) ],
                        kidsInCorral = kidsInCorral environment ++ [KidInCorral( pos element )]
                    }
                _ -> environment


walk:: Environment -> Cell -> Environment
walk env bot = let (jailToGo , dirtToGo) = (nearestCorral env (pos bot), nearestDirt env (pos bot)) in 
    let (bestPathToCorral , bestPathToDirt ) = (fmap (shortestPathToElement env (pos bot)) jailToGo, fmap (shortestPathToElement env (pos bot)) dirtToGo)
    in case (bestPathToCorral , bestPathToDirt) of
        (Just [],Just []) -> env
        (Just [], Just value) -> tryGoToPosition env bot (head value)
        (Nothing , Nothing) -> env
        (Nothing , Just value) -> tryGoToPosition env bot (head value)
        (Just [] , _) -> env
        (Just value , _) -> tryGoToPosition env bot (head value)

shortestPathToElement:: Environment -> Position -> Cell -> [Position]
shortestPathToElement env position element= bfsSearch env position Src.BotWithKid.isWalkable (pos element) 

isWalkable:: Cell-> Bool 
isWalkable element = case (element) of
    (EmptyCell(_,_)) -> True
    (Dirt(_,_)) -> True
    (Corral(_,_)) -> True
    (_) -> False
    