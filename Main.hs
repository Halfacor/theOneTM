module Main where

import TM
import TMExamples
import Test
import TheOneTM

main :: IO ()
main =
  do
    if accepts utm (inputU2 x "a") -- accepts
      then putStrLn "accepts"
      else putStrLn "rejected"
    if accepts utm (inputU2 x "") -- rejects
      then putStrLn "accepts"
      else putStrLn "rejected"
    if accepts utm (inputU2 x "b") -- rejects
      then putStrLn "accepts"
      else putStrLn "rejected"
    -- triple tm
    if accepts utm (inputU2 tripletm "") -- accepts
      then putStrLn "accepts"
      else putStrLn "rejected"
    if accepts utm (inputU2 tripletm "abc") -- accepts
      then putStrLn "accepts"
      else putStrLn "rejected"
    if accepts utm (inputU2 tripletm "aabbcc") -- accepts
      then putStrLn "accepts"
      else putStrLn "rejected"
    if accepts utm (inputU2 tripletm "bc") -- rejects
      then putStrLn "accepts"
      else putStrLn "rejected"
    if accepts utm (inputU2 tripletm "a") -- rejects
      then putStrLn "accepts"
      else putStrLn "rejected"
    if accepts utm (inputU2 tripletm "aab") -- rejects
      then putStrLn "accepts"
      else putStrLn "rejected"