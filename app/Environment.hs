module Environment where 

import Data.List ( sortOn )
import Element
    ( Element(Bot, EmptyCell, Corral, Kid, Obstacle, pos),
      Position,
      first,
      second,
      isNear,
      isElementAtPosition )
import Utils
    ( removeItem, pickRandomInt, pickRandom, (|>), headSafe )

data Environment
  = Environment {
      width :: Int,
      height:: Int,
      bots :: [Element],
      kids:: [Element], 
      jails :: [Element], 
      obstacles :: [Element], 
      dirts :: [Element], 
      botsWithKid :: [Element], 
      kidsInJail :: [Element], 
      empties :: [Element]
  }

toEmptyCell::Element -> Element 
toEmptyCell e = EmptyCell (pos e)

changeEnvironment:: Environment -> Environment
changeEnvironment env = let (oldKids, oldObstacles) = (kids env, obstacles env) in env {
    kids = [],
    obstacles = [],
    empties = empties env ++  map toEmptyCell oldKids ++ map toEmptyCell oldObstacles
  }
  |> changeKidsInEnvironment (length oldKids) 
  |> changeObstaclesInEnvironment (length oldKids)

changeKidsInEnvironment:: Int -> Environment -> Environment
changeKidsInEnvironment amount
  = foldr (.) id (replicate amount createKid)

changeObstaclesInEnvironment:: Int -> Environment -> Environment
changeObstaclesInEnvironment amount
  = foldr (.) id (replicate amount createObstacle)


initEnvironment :: Int -> Int -> Environment
initEnvironment width height =
  Environment {
          width = width,
          height = height,
          bots = [],
          kids = [],
          jails = [],
          obstacles = [],
          dirts = [],
          botsWithKid = [],
          kidsInJail = [],
          empties = [EmptyCell (n,m) | n <- [0 .. width - 1], m <- [0 .. height - 1]]
        }
  |> populateJails |> populateKids |> populateObstacles |> populateBots
       
instance Show Environment where
  show environment =
    (bots environment
    ++ kids environment
    ++ jails environment
    ++ kidsInJail environment
    ++ botsWithKid environment
    ++ obstacles environment
    ++ dirts environment
    ++ empties environment)
    |> sortOn pos
    |> map show
    |> show


--------------------------------------------------------Jails Population--------------------------------------------------------

createJail:: Environment -> Environment
createJail environment
  | null (jails environment) = do
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), jails = [Corral (first emptyCell, second emptyCell)] }
  | otherwise =
    let possibleJail = pickRandom (filter (`hasEmptyAdjacent` environment) (jails environment))
      in let emptyAdjacent = pickRandom (getEmptyAdjacents possibleJail environment)
        in environment {empties = removeItem emptyAdjacent (empties environment), jails= jails environment ++[Corral (pos emptyAdjacent)]}


-- Picks a random number for jailsCount, then compose the function createJail `jailsCount` times with itself.
populateJails:: Environment -> Environment
populateJails environment =
  let jailsCount = pickRandomInt(1, maximum [1 ,(area environment * 25) `div` 100])
    in foldr ($) environment (replicate jailsCount createJail) -- magic!!



-------------------------------------------------------- Kids Population --------------------------------------------------------
createKid:: Environment -> Environment
createKid environment =
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), kids = kids environment ++ [Kid (first emptyCell, second emptyCell)] }



-- Picks the number of jails polulated in environment, then compose the function createKid `jailsCount` times with itself
populateKids:: Environment -> Environment
populateKids environment =
  let jailsCount =  length (jails environment)
    in foldr ($) environment (replicate jailsCount createKid) -- magic again!!


-------------------------------------------------------- Obstacles Population --------------------------------------------------------
createObstacle:: Environment -> Environment
createObstacle environment = do
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), obstacles = obstacles environment ++ [Obstacle (first emptyCell, second emptyCell)] }



-- Picks the number of jails populated in environment, then compose the function createObstacle `obstaclesCount` times with itself
populateObstacles:: Environment -> Environment
populateObstacles environment
  | (area environment * 10) `div` 100 > 0 = 
    let obstaclesCount = pickRandomInt(1, (area environment * 10) `div` 100) 
    in foldr ($) environment (replicate obstaclesCount createObstacle) -- magic again!!
  | otherwise = environment

-------------------------------------------------------- Bots Population --------------------------------------------------------
createBot:: Environment -> Environment
createBot environment = do
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), bots = bots environment ++ [Bot (first emptyCell, second emptyCell)] }



-- Picks the number of jails populated in environment, then compose the function createBot `botsCount` times with itself
populateBots:: Environment -> Environment
populateBots environment =
  let botsCount = pickRandomInt(1, maximum [1 ,(area environment * 10) `div` 100]) 
  in foldr ($) environment (replicate botsCount createBot) -- magic again!!
  


-------------------------------------------------------- Environment utils --------------------------------------------------------

area :: Environment -> Int 
area environment = width environment * height environment


-- Gets the cleaning percentage of the environment. Simulation should be ok if is grater than 60 %
cleanPercentage:: Environment-> Float 
cleanPercentage environment = 
    100.0 -  (dirtsSize / emptyEnvironmentSize)*100
    where dirtsSize = fromIntegral (length (dirts environment)) :: Float
          emptyEnvironmentSize = fromIntegral (length (empties environment) + length (dirts environment)) :: Float

hasEmptyAdjacent:: Element -> Environment -> Bool
hasEmptyAdjacent element environment = any (isNear element) (empties environment)

getEmptyAdjacents:: Element-> Environment-> [Element]
getEmptyAdjacents element environment = filter (isNear element) (empties environment)



getElementAtPosition :: Environment-> Position -> Maybe Element 
getElementAtPosition environment pos = 
  elements environment
  |> filter (isElementAtPosition pos)
  |> headSafe


elements :: Environment -> [Element]
elements environment = bots environment
  ++ kids environment
  ++ jails environment
  ++ kidsInJail environment
  ++ botsWithKid environment
  ++ obstacles environment
  ++ dirts environment
  ++ empties environment
  