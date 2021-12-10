module Main where

import TM
import TMExamples
import Test
import TheOneTM

main :: IO ()
main =
  if accepts utm (inputU2 tripletm "abc")
    then putStrLn "accepts"
    else putStrLn "rejected"