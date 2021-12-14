module Test where

import TM
import TMExamples
import TheOneTM

-- TM tested here:
-- 1) x         - defined by us in TheOneTM.hs
-- 2) tripletm  - defined in the TMExamples

-- for demo:
--original TM x
test_orig1 = accepts x "a" -- return true

test_orig2 = accepts x "b" -- return false
--utm on encoded TM x

ex1 = inputU2 x "a"

ex2 = inputU2 x "b"

test1 = accepts utm ex1 -- return true

test2 = accepts utm ex2 -- return false

-- for testing tripletm
ex = inputU2 tripletm "abc"

test3 = accepts utm ex -- return true!

test4 = accepts utm (inputU2 tripletm "") -- true

test5 = accepts utm (inputU2 tripletm "bc") -- false

initial = initialConfig utm ex -- initial config for TM tripletm

initial1 = initialConfig utm ex1 -- initial config for TM x

-- debugging tool - ntcs
nt = newConfigs utm -- take a config and step the list of next configs
-- obtain the next config (get rid of the bracket since we are having deterministic TM input)

ntc :: Config Integer Char -> Config Integer Char
ntc config = nt config !! 0

-- debugging fxn:
-- input: number of steps as n; the config to begin with
ntcs :: Int -> Config Integer Char -> Config Integer Char
ntcs n config = last $ take (n + 1) (iterate ntc config) -- the first element after iterate function is the current config

--note: the iterate function actually can run to the end:  iterate ntc config
--for example: iterate ntc initial1  -- a little weird though...

-- step1: mark current state q and tape read head S
-- finished marking:
config_mark_qS = ntcs 260 initial1 -- at state 6

-- step2_1: mark the first state q and tape symbol for the first transition
config_mark_trans = ntcs 50 config_mark_qS -- at state 9
-- step2_2: matched

config_matched = ntcs 2600 config_mark_trans -- at state 28

-- step 3: update config
-- 3_1: mark trans and config
config_update = ntcs 400 config_matched -- at state 45
-- 3_2: update finished

config_finish_update = ntcs 8800 config_update -- at state 152

-- step 4: clean up
config_clean = ntcs 3000 config_finish_update

-- step 5: compare and reach the final state:
config_final = ntcs 3390 config_clean -- at state 185 (utm accept state)

-- the utm would check for new config again, but when it fails, it is going to the accept branch
