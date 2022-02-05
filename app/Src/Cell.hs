{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Src.Cell where 

import Core.Utils
import Data.Bifunctor (bimap)
type Position = (Int,Int)

data Cell = 
              Bot{pos::Position}
            | BotAgentV2{pos::Position}
            | Kid{pos::Position}
            | BotWithKid {pos::Position}
            | Dirt {pos::Position}
            | EmptyCell {pos::Position}
            | Corral {pos::Position}
            | KidInCorral {pos::Position}
            | Obstacle {pos::Position}
            deriving (Eq , Ord)

first:: Cell -> Int
first x =  fst (pos x) 

second:: Cell -> Int
second x = snd (pos x)

isNear:: Cell -> Cell -> Bool
isNear e1 e2 = isPosNear (pos e1) (pos e2)

isPosNear:: Position -> Position -> Bool
isPosNear p1 p2 = manhattanDistance p1 p2 <= 1

isElementAtPosition:: Position -> Cell -> Bool
isElementAtPosition position element =
  pos element == position

instance Show Cell where
  show (Bot a) = "B" ++ show a
  show (BotAgentV2 a) = "BV2" ++ show a
  show (Kid a) = "K"++ show a
  show (BotWithKid a) = "BK"++ show a
  show (Dirt a) = "D"++ show a
  show (EmptyCell a) = "E"++ show a
  show (Corral a) = "J"++ show a
  show (KidInCorral a) = "KJ"++ show a
  show (Obstacle a) = "O"++ show a



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
  

add:: Cell -> Direction -> Position 
add element direction = 
    let elementPos = pos element
    in bimap
  (fst elementPos +) (snd elementPos +)
  (directionVector direction)

directionToMove:: Cell -> Position -> Direction
directionToMove element position = 
  let vector = bimap (fst position -) (snd position -) (pos element)
  in fromVector vector
