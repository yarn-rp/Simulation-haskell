module Main where

import Simulation
import Environment

main :: IO ()
main = do 
    print environment
    print simEnvironment
    print cleanPercent