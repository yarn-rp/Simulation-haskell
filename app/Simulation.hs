module Simulation where

import Environment
    ( Environment,
      changeEnvironment,
      initEnvironment,
      cleanPercentage )
import Src.Element ()
import Src.Kid ( moveAll )
import Src.Bot ( moveAll )
import Core.Bfs ()
import Src.BotWithKid ( moveAll )
import Core.Utils ( (|>) )

type Time = Int

totalTime :: Time
totalTime = 30

timeForRandomChange :: Time
timeForRandomChange = 10

environment :: Environment
environment =  initEnvironment 10 10

simEnvironment :: Environment
simEnvironment = simulate environment

cleanPercent :: Float
cleanPercent = cleanPercentage simEnvironment


iteration::(Environment,Time) -> (Environment,Time)
iteration (env,time)
  |  time `mod` timeForRandomChange == 0 = (env |> changeEnvironment |> Src.Bot.moveAll |> Src.BotWithKid.moveAll
   |> Src.Kid.moveAll, 
 time + 1)
  |  otherwise = (env |> Src.Bot.moveAll |> Src.BotWithKid.moveAll |> Src.Kid.moveAll, time + 1)

simulate::Environment -> Environment
simulate env = fst (simulate' (env, 0))

simulate'::(Environment,Time)->(Environment,Time)
simulate' (env, time) 
  | time == totalTime = (env,time)
  | otherwise = (env,time) |> iteration |> simulate'
