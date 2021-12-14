module Test where

import TM
import TMExamples
import TheOneTM

-- 1) x         - defined by us in TheOneTM.hs
-- 2) tripletm  - defined in the TMExamples

-- for demo:
--original TM x
test_orig1 = accepts x "a" -- return true

test_orig2 = accepts x "b" -- return false
--utm on encoded TM x

ex1 = inputU2 x "a"

ex2 = inputU2 x "b"

initial1 = initialConfig utm ex1 -- initial config for TM x

-- for testing tripletm
ex = inputU2 tripletm "abc"

initial = initialConfig utm ex -- initial config for TM tripletm

-- debugging tool - ntcs
nt = newConfigs utm -- take a config and step the list of next configs
-- obtain the next config (get rid of the bracket since we are having deterministic UTM)

ntc :: Config Integer Char -> Config Integer Char
ntc config = head (nt config)

-- debugging fxn:
-- input: number of steps as n; the config to begin with
--note: the iterate function actually can run to the end:  iterate ntc config
--for example: iterate ntc initial1  -- a little weird though...
ntcs :: Int -> Config Integer Char -> Config Integer Char
ntcs n config = last $ take (n + 1) (iterate ntc config) -- the first element after iterate function is the current config

-- input: number of steps and the input tape
ntcsFromStart :: Int -> [Char] -> Config Integer Char
ntcsFromStart n input = ntcs n (initialConfig utm input)

-- step1: mark current state q and tape read head S
-- finished marking:
config_mark_qS = ntcs 155 initial1 -- at state 5

-- step2_1: mark the first state q and tape symbol for the first transition
config_mark_trans = ntcs 127 config_mark_qS -- at state 9

-- step2_2: matched
config_matched = ntcs 2602 config_mark_trans -- at state 26

-- step 3: update config
-- 3_1: mark trans and config
config_update = ntcs 384 config_matched -- at state 44

-- 3_2: update finished
config_finish_update = ntcs 11666 config_update -- at state 156

-- step 4: clean up
config_clean = ntcs 188 config_finish_update

-- step 5: no matching transition, go check final
config_final = ntcs 2067 config_clean

-- step 6: accepted
config_end = ntcs 1311 config_final

-- the utm would check for new config again, but when it fails, it is going to the accept branch
