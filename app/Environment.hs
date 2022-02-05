module Environment where 

import Data.List ( sortOn )

import Core.Utils
    ( removeItem, pickRandomInt, pickRandom, (|>), headSafe )
import Src.Cell
    ( Cell(BotAgentV2, EmptyCell, Corral, Kid, Obstacle, pos),
      Position,
      first,
      second,
      isNear,
      isElementAtPosition ) 

data Environment
  = Environment {
      bots :: [Cell],
      kids:: [Cell], 
      jails :: [Cell], 
      obstacles :: [Cell], 
      dirts :: [Cell], 
      botsWithKid :: [Cell], 
      kidsInCorral :: [Cell], 
      empties :: [Cell],
      width :: Int,
      height:: Int
  }


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
          kidsInCorral = [],
          empties = [EmptyCell (n,m) | n <- [0 .. width - 1], m <- [0 .. height - 1]]
        }
  |> genCorrals |> genKids |> genObstacles |> genBots
       
instance Show Environment where
  show environment =
    (bots environment
    ++ kids environment
    ++ jails environment
    ++ kidsInCorral environment
    ++ botsWithKid environment
    ++ obstacles environment
    ++ dirts environment
    ++ empties environment)
    |> sortOn pos
    |> map show
    |> show

toEmptyCell::Cell -> Cell 
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


createCorral:: Environment -> Environment
createCorral environment
  | null (jails environment) = do
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), jails = [Corral (first emptyCell, second emptyCell)] }
  | otherwise =
    let possibleCorral = pickRandom (filter (`hasEmptyAdjacent` environment) (jails environment))
      in let emptyAdjacent = pickRandom (getEmptyAdjacents possibleCorral environment)
        in environment {empties = removeItem emptyAdjacent (empties environment), jails= jails environment ++[Corral (pos emptyAdjacent)]}


genCorrals:: Environment -> Environment
genCorrals environment =
  let jailsCount = pickRandomInt(1, maximum [1 ,(area environment * 25) `div` 100])
    in foldr ($) environment (replicate jailsCount createCorral) -- magic!!



createKid:: Environment -> Environment
createKid environment =
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), kids = kids environment ++ [Kid (first emptyCell, second emptyCell)] }



genKids:: Environment -> Environment
genKids environment =
  let jailsCount =  length (jails environment)
    in foldr ($) environment (replicate jailsCount createKid)


createObstacle:: Environment -> Environment
createObstacle environment = do
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), obstacles = obstacles environment ++ [Obstacle (first emptyCell, second emptyCell)] }

genObstacles:: Environment -> Environment
genObstacles environment
  | (area environment * 10) `div` 100 > 0 = 
    let obstaclesCount = pickRandomInt(1, (area environment * 10) `div` 100) 
    in foldr ($) environment (replicate obstaclesCount createObstacle) 
  | otherwise = environment

createBot:: Environment -> Environment
createBot environment = do
    let emptyCell = pickRandom (empties environment) in environment {empties = removeItem emptyCell (empties environment), bots = bots environment ++ [BotAgentV2 (first emptyCell, second emptyCell)] }

genBots:: Environment -> Environment
genBots environment =
  let botsCount = pickRandomInt(1, maximum [1 ,(area environment * 10) `div` 100]) 
  in foldr ($) environment (replicate botsCount createBot) 
  
area :: Environment -> Int 
area environment = width environment * height environment

cleanPercentage:: Environment-> Float 
cleanPercentage environment = 
    100.0 -  (dirtsSize / emptyEnvironmentSize)*100
    where dirtsSize = fromIntegral (length (dirts environment)) :: Float
          emptyEnvironmentSize = fromIntegral (length (empties environment) + length (dirts environment)) :: Float

hasEmptyAdjacent:: Cell -> Environment -> Bool
hasEmptyAdjacent element environment = any (isNear element) (empties environment)

getEmptyAdjacents:: Cell-> Environment-> [Cell]
getEmptyAdjacents element environment = filter (isNear element) (empties environment)

getElementAtPosition :: Environment-> Position -> Maybe Cell 
getElementAtPosition environment pos = 
  elements environment
  |> filter (isElementAtPosition pos)
  |> headSafe

elements :: Environment -> [Cell]
elements environment = bots environment
  ++ kids environment
  ++ jails environment
  ++ kidsInCorral environment
  ++ botsWithKid environment
  ++ obstacles environment
  ++ dirts environment
  ++ empties environment
  