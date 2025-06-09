module Main where

import           System.Environment (getArgs)
import           Text.Printf        (printf)

import qualified Quest01.Solve      as Q01
import qualified Quest02.Solve      as Q02

solve :: String -> String -> IO ()
solve "quest01" part = Q01.solve part
solve "quest02" part = Q02.solve part

solve quest part     = printf "Wrong input: quest=`%s` part=`%s`\n" quest part

main :: IO ()
main = do
    quest:part:_ <- getArgs
    solve quest part
