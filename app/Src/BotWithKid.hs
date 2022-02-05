module Src.BotWithKid where 
import Environment
    ( Environment(dirts, empties, jails, botsWithKid, bots,
                  kidsInJail),
      getElementAtPosition )
import Src.Element
    ( isDirtInt ,Element( Corral, BotWithKid, Bot, KidInJail, EmptyCell, Dirt,
              pos),
      Position )
import Core.Utils ( removeItem, manhattanDistance, (|>), headSafe )
import Data.List ( sortOn )
import Core.Bfs ( bfsSearch )


moveAll:: Environment -> Environment
moveAll environment = 
    foldl Src.BotWithKid.move environment (botsWithKid environment)

sortTuplesDistances :: Ord a1 => (a2, a1) -> (a3, a1) -> Ordering
sortTuplesDistances (a1, b1) (a2, b2) = compare b1 b2

nearestJail:: Environment -> Position -> Maybe Element
nearestJail env pos = nearestPositionInCollection env pos (jails env)
    
nearestDirt:: Environment -> Position -> Maybe Element
nearestDirt env pos = nearestPositionInCollection env pos (dirts env)

nearestPositionInCollection::Environment -> Position  -> [Element] -> Maybe Element
nearestPositionInCollection env initialPos collection = let elementWithDistances = map (\element -> (element, manhattanDistance initialPos (pos element)) ) collection
    in sortOn snd elementWithDistances
    |> headSafe 
    |> fmap fst
  

tryGoToPosition:: Environment -> Element -> Position -> Environment
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
                        kidsInJail = kidsInJail environment ++ [KidInJail( pos element )]
                    }
                _ -> environment


move:: Environment -> Element -> Environment
move env bot = let (jailToGo , dirtToGo) = (nearestJail env (pos bot), nearestDirt env (pos bot)) in 
    let (bestPathToJail , bestPathToDirt ) = (fmap (shortestPathToElement env (pos bot)) jailToGo, fmap (shortestPathToElement env (pos bot)) dirtToGo)
    in case (bestPathToJail , bestPathToDirt) of
        (Just [],Just []) -> env
        (Just [], Just value) -> tryGoToPosition env bot (head value)
        (Nothing , Nothing) -> env
        (Nothing , Just value) -> tryGoToPosition env bot (head value)
        (Just [] , _) -> env
        (Just value , _) -> tryGoToPosition env bot (head value)


-------------------------------------------------Smart stuff------------------------------------------------------------


-- Gets the dirtiness(amount of dirty elements) of a given path
dirtyness:: Environment -> [Position] -> Int
-- dirtyness env path = sum (map (isDirtInt (.) getElementAtPosition) path)
--TODO: check if this works, porke ta mas chula
dirtyness env path = sum (map (\a -> isDirtInt (getElementAtPosition env a)) path)

shortestPathToElement:: Environment -> Position -> Element -> [Position]
shortestPathToElement env position element= bfsSearch env position Src.BotWithKid.isWalkable (pos element) 

isWalkable:: Element-> Bool 
isWalkable element = case (element) of
    (EmptyCell(_,_)) -> True
    (Dirt(_,_)) -> True
    (Corral(_,_)) -> True
    (_) -> False
    