module Simulation where

import Environment
    ( Environment,
      changeEnvironment,
      initEnvironment,
      cleanPercentage )
import Element ()
import Kid ( moveAll )
import Bot ( moveAll )
import BFS ()
import BotWithKid ( moveAll )
import Utils ( (|>) )

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
  |  time `mod` timeForRandomChange == 0 = (env |> changeEnvironment |> Bot.moveAll |> BotWithKid.moveAll
   |> Kid.moveAll, 
 time + 1)
  |  otherwise = (env |> Bot.moveAll |> BotWithKid.moveAll |> Kid.moveAll, time + 1)

simulate::Environment -> Environment
simulate env = fst (simulate' (env, 0))

simulate'::(Environment,Time)->(Environment,Time)
simulate' (env, time) 
  | time == totalTime = (env,time)
  | otherwise = (env,time) |> iteration |> simulate'
