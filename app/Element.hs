{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Element where 

import Utils
import Data.Bifunctor (bimap)
type Position = (Int,Int)

data Element = 
              Bot{pos::Position}
            | BotAgentV2{pos::Position}
            | Kid{pos::Position}
            | BotWithKid {pos::Position}
            | Dirt {pos::Position}
            | EmptyCell {pos::Position}
            | Corral {pos::Position}
            | KidInJail {pos::Position}
            | Obstacle {pos::Position}
            deriving (Eq , Ord)

first:: Element -> Int
first x =  fst (pos x) 

second:: Element -> Int
second x = snd (pos x)

isNear:: Element -> Element -> Bool
isNear e1 e2 = isPosNear (pos e1) (pos e2)

isPosNear:: Position -> Position -> Bool
isPosNear p1 p2 = manhattanDistance p1 p2 <= 1

isElementAtPosition:: Position -> Element -> Bool
isElementAtPosition position element =
  pos element == position

instance Show Element where
  show (Bot a) = "B" ++ show a
  show (BotAgentV2 a) = "BV2" ++ show a
  show (Kid a) = "K"++ show a
  show (BotWithKid a) = "BK"++ show a
  show (Dirt a) = "D"++ show a
  show (EmptyCell a) = "E"++ show a
  show (Corral a) = "J"++ show a
  show (KidInJail a) = "KJ"++ show a
  show (Obstacle a) = "O"++ show a


--------------------------------------------------------Directions--------------------------------------------------------

data Direction = N | S | E | W
    deriving (Show,Eq)

directionVector:: Direction -> (Int,Int)
directionVector N = (1,0)
directionVector S = (-1,0)
directionVector W = (0,-1)
directionVector E = (0,1)

fromVector :: (Int,Int) -> Direction
fromVector (n,_)
  | n > 0 = N
  | n <= 0 = S
fromVector (_,m)
  | m < 0 = W
  | m >= 0 = E
  

add:: Element -> Direction -> Position 
add element direction = 
    let elementPos = pos element
    in bimap
  (fst elementPos +) (snd elementPos +)
  (directionVector direction)

directionToMove:: Element -> Position -> Direction
directionToMove element position = 
  let vector = bimap (fst position -) (snd position -) (pos element)
  in fromVector vector


--Similar to isDirty, but returns an int
isDirtInt:: Maybe Element -> Int
isDirtInt element = case element of 
            Nothing -> 0
            Just element ->
                case element of
                    (Dirt (_,_)) -> 1 
                    _ -> 0