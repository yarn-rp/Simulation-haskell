module Core.Bfs where

import           Control.Monad.State
import qualified Data.Array as A
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Data.List
import Src.Element ( Element(Dirt), Position )
import Environment
    ( Environment(width, height), getElementAtPosition )
import Core.Utils

type Grid = Environment

bfsSearch :: Grid -> Position -> (Element -> Bool) -> Position -> [Position]
bfsSearch grid start isWalkableCondition finish = evalState (bfsSearch' grid finish isWalkableCondition) initialState
  where
    initialState = BFSState (S.singleton start) Set.empty M.empty

data BFSState = BFSState
  { queue :: S.Seq Position
  , visited :: Set.Set Position
  , parents :: M.Map Position Position
  } deriving (Show)

bfsSearch' :: Grid -> Position -> (Element -> Bool) -> State BFSState [Position]
bfsSearch' grid finish isWalkableCondition = do
  (BFSState q v p) <- get
  case S.viewl q of
    (top S.:< rest) -> if top == finish
      then return (unwindPath p [finish])
      else do
        let validAdjacent = getValidNeighbors top grid isWalkableCondition v 
        let newQueue = foldr (flip (S.|>)) rest validAdjacent
        let newVisited = Set.insert top v
        let newParentsMap = foldr (`M.insert` top) p validAdjacent
        put (BFSState newQueue newVisited newParentsMap)
        bfsSearch' grid finish isWalkableCondition
    _ -> return []

unwindPath :: M.Map Position Position -> [Position] -> [Position]
unwindPath parentsMap currentPath = case M.lookup (head currentPath) parentsMap of
  Nothing -> tail currentPath
  Just parent -> unwindPath parentsMap (parent : currentPath)


isWalkable':: Environment -> Position  -> (Element -> Bool)-> Bool
isWalkable' env pos isWalkableCondition = let element = getElementAtPosition env pos in
    maybe False isWalkableCondition element

putDirtFirst:: Environment -> Position -> Ordering
putDirtFirst env pos =
  let element = getElementAtPosition env pos in
    case element of 
        Nothing -> LT
        Just element -> case element of
            (Dirt(_,_)) -> GT
            _ -> LT

getValidNeighbors :: Position -> Grid -> (Element->Bool) -> Set.Set Position -> [Position]
getValidNeighbors (r, c) grid isWalkableCondition v = catMaybes [right', down', left', up']
  where
    (rowMax, colMax) = (width grid, height grid)
    right = (r, c + 1)
    right' = if c + 1 <= colMax && not (Set.member right v) && isWalkable' grid right isWalkableCondition
      then Just right
      else Nothing
    down = (r + 1, c)
    down' = if r + 1 <= rowMax && not (Set.member down v)&& isWalkable' grid down isWalkableCondition
      then Just down
      else Nothing
    left = (r, c - 1)
    left' = if c - 1 >= 0 && not (Set.member left v) && isWalkable' grid left isWalkableCondition
      then Just left
      else Nothing
    up = (r - 1, c)
    up' = if r - 1 >= 0 && not (Set.member up v) && isWalkable' grid up isWalkableCondition
      then Just up
      else Nothing