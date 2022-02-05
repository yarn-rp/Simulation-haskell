module Simulation where

import Environment
    ( Environment,
      changeEnvironment,
      initEnvironment,
      cleanPercentage )
import Src.Cell ()
import Src.Kid ( walkAll )
import Src.Bot ( walkAll )
import Core.Bfs ()
import Src.BotWithKid ( walkAll )
import Core.Utils ( (|>) )

type Time = Int


environment :: Environment
environment =  initEnvironment 7 7

simEnvironment :: Environment
simEnvironment = simulate environment

cleanPercent :: Float
cleanPercent = cleanPercentage simEnvironment

totalTime :: Time
totalTime = 25

timeForRandomChange :: Time
timeForRandomChange = 5

simulate::Environment -> Environment
simulate env = fst (sim (env, 0))

sim::(Environment,Time)->(Environment,Time)
sim (env, time) 
  | time == totalTime = (env,time)
  | otherwise = (env,time) |> iter |> sim

iter::(Environment,Time) -> (Environment,Time)
iter (env,time)
  |  time `mod` timeForRandomChange == 0 = (env |> changeEnvironment |> Src.Bot.walkAll |> Src.BotWithKid.walkAll
   |> Src.Kid.walkAll, 
 time + 1)
  |  otherwise = (env |> Src.Bot.walkAll |> Src.BotWithKid.walkAll |> Src.Kid.walkAll, time + 1)
