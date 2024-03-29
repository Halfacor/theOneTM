module TMExamples where

-- import UniversalTM

-- import Backup
import TM

-- import TheOneTM

-- following suggestion by Junnan in class
tripletm =
  TM [1 .. 6] "abc" "abc*! " id ' ' '!' trans 1 [6]
  where
    trans =
      goRight 1 ' ' ' ' 6
        ++ loopRight 1 "*"
        ++ goRight 1 'a' '*' 2
        ++ loopRight 2 "a*"
        ++ goRight 2 'b' '*' 3
        ++ loopRight 3 "b*"
        ++ goRight 3 'c' '*' 4
        ++ loopRight 4 "c*"
        ++ goLeft 4 ' ' ' ' 5
        ++ loopLeft 5 "abc*"
        ++ goRight 5 '!' '!' 1

x =
  TM [1, 2] "a" "a !" id ' ' '!' t2 1 [2]
  where
    t2 = goRight 1 'a' 'a' 2

trans2 =
  goRight 1 ' ' ' ' 6
    ++ loopRight 1 "*"
    ++ goRight 1 'a' '*' 2
    ++ loopRight 2 "a*"
    ++ goRight 2 'b' '*' 3
    ++ loopRight 3 "b*"
    ++ goRight 3 'c' '*' 4
    ++ loopRight 4 "c*"
    ++ goLeft 4 ' ' ' ' 5
    ++ loopLeft 5 "abc*"
    ++ goRight 5 '!' '!' 1