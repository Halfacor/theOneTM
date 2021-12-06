module TheOneTM where

import Data.Char
import Data.List
import TM
import Util
import TMExamples


uTape :: String
uTape = " !;:#$,._tuvwxyz10"

-- encode base on its order in the input tape
-- offset is 1 when encoding state
class UEncode a where
  bitenc :: a -> String -- over utape

instance UEncode Bool where
  bitenc True = "1_"
  bitenc False = "0_"

--states
instance UEncode Int where
  bitenc x =
    if x <= 0
      then []
      else bitenc (x `div` 2) ++ bitenc (x `mod` 2 == 1)

instance UEncode Integer where
  bitenc x = bitenc (fromIntegral x :: Int)

-- tape
instance UEncode Char where
  bitenc x = bitenc $ ord x

instance UEncode Direction where
  bitenc GoLeft = "1_"
  bitenc GoRight = "0_"

{- convenience function to encode a, and then append another string.
    The Char u separates the two strings.
-}
bitencPre :: UEncode a => String -> a -> String -> String
bitencPre u x rest = u ++ bitenc x ++ rest

colon :: UEncode a => a -> String -> String
colon = bitencPre ":_"

comma :: UEncode a => a -> String -> String
comma = bitencPre ","

dollar :: UEncode a => a -> String -> String
dollar = bitencPre "$_"

dot :: UEncode a => a -> String -> String
dot = bitencPre "._"

commaDot :: UEncode a => a -> String -> String
commaDot = bitencPre ",._"

pound :: UEncode a => a -> String -> String
pound = bitencPre "#"

poundDot :: UEncode a => a -> String -> String
poundDot = bitencPre "#._"

semiColon :: UEncode a => a -> String -> String
semiColon = bitencPre ";_"

instance (UEncode state, UEncode tape) => UEncode (Trans state tape) where
  bitenc (Trans st g d st' g') =
    (bitenc st ++) $
      commaDot g $
        commaDot g' $
          comma d $
            comma st' ""

-- https://stackoverflow.com/questions/9220986/is-there-any-haskell-function-to-concatenate-list-with-separator
listH :: UEncode a => [a] -> String -> String
listH xs rest = concatMap (\a -> ";_" ++ bitenc a) xs ++ rest

encodeh ::
  (UEncode input, UEncode state, UEncode tape) =>
  TM input state tape ->
  String {- string to follow this one -} ->
  String -- over Utape
encodeh (TM states inputs tapesyms _ blank leftend trans start finals) rest =
  --   dot leftend $
  -- poundDot blank $
  dot blank $
    --      dot start $
    ("$_" ++) $
      (intercalate "$_" (map bitenc finals) ++) $ -- final states
        listH trans rest -- transitions/program descriptions

encode ::
  (UEncode input, UEncode state, UEncode tape) =>
  TM input state tape ->
  String -- over Utape
encode tm = encodeh tm ""

-- -- turn a TM and an input string into a single input for the universal TM
-- inputU ::
--   (UEncode input, UEncode state, UEncode tape) =>
--   TM input state tape ->
--   [input] ->
--   String -- over Utape
-- inputU tm xs = encodeh tm (":_:_" ++ bitenc (leftend tm) ++ "._," ++ bitenc (start tm) ++ "," ++ tmp) -- initial config
--   where
--     tmp = case length xs of
--       1 -> bitenc (head xs) ++ ".," ++ bitenc (blank tm)
--       0 -> bitenc (blank tm) ++ ".," ++ bitenc (blank tm)
--       _ -> bitenc (head xs) ++ ".," ++ concatMap (\x -> bitenc x ++ ".") (tail xs)

inputU2 ::
  (UEncode input, UEncode state, UEncode tape) =>
  TM input state tape ->
  [input] ->
  String -- over Utape
inputU2 tm xs = encodeh tm (":_:_" ++ dot (leftend tm) (comma (start tm) tmp))
  where
    tmp = case length xs of
      1 -> commaDot (head xs) (commaDot (blank tm) "")
      0 -> commaDot (blank tm) (commaDot (blank tm) "")
      _ -> commaDot (head xs) ("," ++ concatMap (\x -> "._" ++ bitenc x) (tail xs))

--------------------------------------------------------------------------------
ex = inputU2 tripletm "abc"
--------------------------------------------------------------------------------
-- remove elements from the list
r :: Eq a => a -> [a] -> [a]
r _ []                 = []
r x (y:ys) | x == y    = r x ys
                    | otherwise = y : r x ys

-- remove list of chars (strings) from the tape symbols
re [] tape     = tape
re (x:xs) tape = re xs (r x tape)

-- 1st tape is the new tape symbol, the 2nd arg - [tape] is the list of old symbols
loop1 :: Direction -> state -> tape ->[tape] -> [Trans state tape]
loop1 d st newtape = (map (\ g -> Trans st g d st newtape))


loopRight1 :: state -> tape -> [tape] -> [Trans state tape]
loopRight1 = loop1 GoRight

loopLeft1 :: state -> tape -> [tape] -> [Trans state tape]
loopLeft1 = loop1 GoLeft



tape = ".01$;,:yutwvxz! _"

transition = loopRight 1 (re " " tape)
 ++ goLeft 1 ' ' ' ' 2
 ++ loopLeft 2 (re "," tape)    -- loop without changing symbols -- loop
 ++ goLeft 2 ',' ',' 3
 ++ loopLeft1 3 '_' "y"      -- loop with changing symbols -- loop 1
 ++ loopLeft 3 (re "_," tape) 
 ++ goLeft 3 ',' ',' 4
 ++ loopLeft1 4 '_' "y"
 ++ loopLeft 4 (re "," tape)
 ++ goLeft 4 ',' ',' 5
 ++ loopLeft 5 (re "!" tape)
 ++ goRight 5 '!' '!' 6
 ++ loopRight 6 (re "; " tape)
 ++ goRight 6 ';' ';' 190
 ++ goLeft 6 ' ' ' ' 165                -- TO THE ACCEPT PATH
 ++ goRight 190 'z' 'z' 6
 ++ goRight 190 '_' '_' 7
 ++ loopRight 7 (re "_," tape)
 ++ loopRight1 7 'x' "_"
 ++ goRight 7 ',' ',' 8
 ++ loopRight 8 (re "_," tape)
 ++ loopRight1 8 'x' "_"
 
 --compare
 ++ goRight 8 ',' ',' 9
 ++ loopLeft 9 (re "!" tape)
 ++ goRight 9 '!' '!' 10
 ++ loopRight 10 (re "_y" tape)
 ++ goLeft 10 ' ' ' ' 21
 ++ goLeft 10 'y' '_' 11
 ++ goLeft 11 '0' '0' 12
 ++ goLeft 11 '.' '.' 13
 ++ goLeft 11 '1' '1' 14
 -- left branch
 ++ loopLeft 12 (re "!" tape)
 ++ goRight 12 '!' '!' 15
 ++ loopRight 15 (re "_x" tape)
 ++ goLeft 15 'x' '_' 18
 ++ goLeft 15 ' ' ' ' 23
 -- ++ goLeft 18 --> 22             -- not sure
 ++ goLeft 18 '0' '0' 9
 -- middle branch
 ++ loopLeft 13 (re "!" tape)
 ++ goRight 13 '!' '!' 16
 ++ loopRight 16 (re " x" tape)
 ++ goLeft 16 ' ' ' ' 23
 ++ goLeft 16 'x' ' ' 19
 -- ++ 19 --> 22                       -- not sure
 ++ goLeft 19 '.' '.' 9
 -- right branch
 ++ loopLeft 14 (re "!" tape)
 ++ goRight 14 '!' '!' 17
 ++ loopRight 17 (re " x" tape)
 ++ goLeft 17 ' ' ' ' 23
 ++ goLeft 17 'x' ' ' 20
 -- ++ 20 --> 22                         -- not sure
 ++ goLeft 20 '1' '1' 9
 -- compare fail:
 ++ loopLeft 21 (re "!x" tape)
 ++ goLeft 21 'x' '_' 22
 ++ loopRight 22 (re " " tape)
 ++ goLeft 22 ' ' ' ' 23
 ++ loopLeft1 23 '_' "x"
 ++ loopLeft1 23 '_' "y"
 ++ loopLeft 23 (re "xy!" tape)
 ++ goRight 23 '!' '!' 24
 ++ loopRight 24 (re ";" tape)
 ++ goRight 24 ';' ';' 25
 ++ goRight 25 ';' ';' 24
 ++ goRight 25 '_' 'z' 1
 
 
 --compare suceed:
 ++ goRight 22 '!' '!' 26
 ++ loopRight 26 (re ";" tape)
 ++ goRight 26 ';' ';' 27
 ++ goRight 27 'z' 'z' 26
 ++ loopRight 27 (re "," tape)
 ++ goRight 27 ',' ',' 28
 ++ loopRight 28 (re "," tape)
 ++ goRight 28 ',' ',' 29
 ++ loopRight1 29 'u' "_"
 ++ loopRight 29 (re "_," tape)
 ++ goRight 29 ',' ',' 30
 ++ loopRight1 30 'u' "_"
 ++ loopRight 30 (re "_," tape)
 ++ goRight 30 ',' ',' 31
 ++ loopRight1 31 'y' "_"
 ++ loopRight 31 (re "_," tape)
 ++ goLeft 31 ',' ',' 32
 ++ loopLeft 32 (re "z!" tape)
 ++ loopLeft1 32 '_' "z"
 ++ goRight 32 '!' '!' 33
 ++ loopRight 33 " "
 ++ goLeft 33 ' ' ' ' 34
 ++ loopLeft 34 (re ":" tape)
 ++ goRight 34 ':' ':' 35
 ++ goRight 35 '_' '_' 36
 ++ loopRight 36 (re "," tape)
 ++ loopRight1 36 'v' "_"
 ++ goLeft 36 ',' ',' 37
 ++ loopLeft1 37 'x' "v"
 ++ loopLeft 37 (re "v_." tape)
 ++ goRight 37 '_' '_' 38
 ++ goRight 37 '.' '.' 38
 ++ loopRight 38 (re "," tape)
 ++ goRight 38 ',' ',' 39
 ++ loopRight 39 (re "," tape)
 ++ goRight 39 ',' ',' 40
 ++ loopRight 40 (re "," tape)
 ++ goRight 40 ',' ',' 41
 ++ goRight 41 '.' '.' 42
 ++ loopRight1 42 'z' "_"
 ++ loopRight 42 (re " _." tape)
 ++ goRight 42 '.' '.' 43
 ++ goRight 42 ' ' ':' 44
 ++ loopRight1 43 'w' "_"
 ++ loopRight 43 (re " _" tape)
 ++ goRight 43 ' ' ':' 44
 ++ goRight 44 ' ' '_' 45
 ++ loopLeft 45 (re "!u" tape)
 ++ goLeft 45 'u' '_' 46
 
 -- update config - PqSQ
 ++ goRight 46 '1' '1' 47
 ++ goRight 46 '0' '0' 99
 
 -- top branch
 ++ loopRight 47 (re "v_" tape)
 ++ goLeft 47 ' ' ',' 56
 ++ goLeft 47 'v' '_' 48
 ++ goRight 48 '0' '0' 49
 -- ++ 48-->50                              --missing
 ++ goRight 48 '.' '.' 50
 ++ goRight 48 '1' '1' 51
 ++ loopLeft 49 (re " " tape)
 ++ goLeft 49 ' ' '0' 52
 ++ goLeft 52 ' ' '_' 55
 ++ loopLeft 50 (re " " tape)
 ++ goLeft 50 ' ' '.' 53
 ++ goLeft 53 ' ' '_' 55
 ++ loopLeft 51 (re " " tape)
 ++ goLeft 51 ' ' '1' 54
 ++ goLeft 54 ' ' '_' 55
 ++ loopLeft 55 (re "!" tape)
 ++ loopLeft 56 (re "!" tape)
 ++ goRight 56 '!' '!' 57
 ++ loopRight 57 (re "y_" tape)
 ++ goLeft 57 'y' '_' 58
 ++ goLeft 57 ' ' ',' 63
 ++ goRight 58 '0' '0' 59
 ++ goRight 58 '1' '1' 60
 ++ loopRight 59 (re " " tape)
 ++ goLeft 59 ' ' '0' 61
 ++ goLeft 61 ' ' '_' 56
 ++ loopRight 60 (re " " tape) 
 ++ goLeft 60 ' ' '1' 62
 ++ goLeft 62 ' ' '_' 56
 ++ loopLeft 63 (re "!" tape)
 ++ goRight 63 '!' '!' 64
 ++ loopRight 64 (re "x_" tape)
 ++ goLeft 64 ' ' ',' 72
 ++ goLeft 64 'x' '_' 65
 ++ goRight 65 '0' '0' 66
 -- ++ 65 --> 67                               --missing
 ++ goRight 65 '.' '.' 67
 ++ goRight 65 '1' '1' 68
 ++ loopLeft 66 (re " " tape)
 ++ goLeft 66 ' ' '0' 69
 ++ goLeft 69 ' ' '_' 63
 ++ loopLeft 67 (re " " tape)
 ++ goLeft 67 ' ' '.' 70
 ++ goLeft 70 ' ' '_' 63
 ++ loopLeft 68 (re " " tape)
 ++ goLeft 68 ' ' '1' 71
 ++ goLeft 71 ' ' '_' 63
 ++ loopLeft 72 (re "!" tape)
 ++ goRight 72 '!' '!' 73
 ++ loopRight 73 (re "u " tape)
 ++ goLeft 73 'u' '_' 74
 ++ goLeft 73 ' ' ' ' 81
 ++ goRight 74 '0' '0' 75
 -- ++ 74 --> 76                                --missing
 ++ goRight 74 '.' '.' 76
 ++ goRight 74 '1' '1' 77
 ++ loopLeft 75 (re " " tape) 
 ++ goLeft 75 ' ' '0' 78
 ++ goLeft 78 ' ' '_' 72
 ++ loopLeft 76 (re " " tape)
 ++ goLeft 76 ' ' '.' 79
 ++ goLeft 79 ' ' '_' 72
 ++ loopLeft 77 (re " " tape)
 ++ goLeft 77 ' ' '1' 80
 ++ goLeft 80 ' ' '_' 72
 
 ++ loopLeft 81 (re "!" tape)
 ++ goRight 81 '!' '!' 82
 ++ loopRight 82 (re "z " tape)
 ++ goRight 82 ' ' ' ' 90
 ++ goLeft 82 'z' '_' 83
 ++ goRight 83 '0' '0' 84
 -- ++ 83 --> 85                                --missing
 ++ goRight 83 '.' '.' 85
 ++ goRight 83 '1' '1' 86
 ++ loopLeft 84 (re " " tape) 
 ++ goLeft 84 ' ' '0' 87
 ++ goLeft 87 ' ' '_' 81
 ++ loopLeft 85 (re " " tape)
 ++ goLeft 85 ' ' '.' 88
 ++ goLeft 88 ' ' '_' 81
 ++ loopLeft 86 (re " " tape)
 ++ goLeft 86 ' ' '1' 89
 ++ goLeft 89 ' ' '_' 81
  
 ++ loopLeft 90 (re "!" tape)
 ++ goRight 90 '!' '!' 91
 ++ loopRight 91 (re "w " tape)
 ++ goRight 91 ' ' ' ' 164      --next page
 ++ goLeft 91 'w' '_' 92
 ++ goRight 92 '0' '0' 93
 -- ++ 92 --> 94                                --missing
 ++ goRight 92 '.' '.' 94
 ++ goRight 92 '1' '1' 95
 ++ loopLeft 93 (re " " tape) 
 ++ goLeft 93 ' ' '0' 96
 ++ goLeft 96 ' ' '_' 90
 ++ loopLeft 94 (re " " tape)
 ++ goLeft 94 ' ' '.' 97
 ++ goLeft 97 ' ' '_' 90
 ++ loopLeft 95 (re " " tape)
 ++ goLeft 95 ' ' '1' 98
 ++ goLeft 98 ' ' '_' 90
 
 -- lower branch:
 ++ loopRight 99 (re "v_" tape)
 ++ goLeft 99 ' ' ',' 108
 ++ goLeft 99 'v' '_' 100
 ++ goRight 100 '0' '0' 101
 -- ++ 100-->102                              --missing
 ++ goRight 100 '.' '.' 102
 ++ goRight 100 '1' '1' 103
 ++ loopLeft 101 (re " " tape)
 ++ goLeft 101 ' ' '0' 104
 ++ goLeft 104 ' ' '_' 107
 ++ loopLeft 102 (re " " tape)
 ++ goLeft 102 ' ' '.' 105
 ++ goLeft 105 ' ' '_' 107
 ++ loopLeft 103 (re " " tape)
 ++ goLeft 103 ' ' '1' 106
 ++ goLeft 106 ' ' '_' 107
 ++ loopLeft 107 (re "!" tape)
 ++ loopLeft 108 (re "!" tape)
 ++ goRight 108 '!' '!' 109
 ++ loopRight 109 (re "x_" tape)
 ++ goLeft 109 'x' '_' 110
 ++ goLeft 109 ' ' ',' 117
 ++ goRight 110 '0' '0' 111
 -- ++ 110-->112                              --missing
 ++ goRight 110 '.' '.' 112
 ++ goRight 110 '1' '1' 113
 ++ loopRight 111 (re " " tape)
 ++ goLeft 111 ' ' '0' 114
 ++ goLeft 114 ' ' '_' 108
 ++ goLeft 112 ' ' '.' 115
 ++ goLeft 115 ' ' '_' 108 
 ++ loopRight 113 (re " " tape) 
 ++ goLeft 113 ' ' '1' 116
 ++ goLeft 116 ' ' '_' 108
 ++ loopLeft 117 (re "!" tape)
 ++ goRight 117 '!' '!' 118
 ++ loopRight 118 (re "u_" tape)
 ++ goLeft 118 ' ' ',' 126
 ++ goLeft 118 'u' '_' 119
 ++ goRight 119 '0' '0' 120
 -- ++ 119 --> 121                               --missing
 ++ goRight 119 '.' '.' 121
 ++ goRight 119 '1' '1' 122
 ++ loopLeft 120 (re " " tape)
 ++ goLeft 120 ' ' '0' 123
 ++ goLeft 123 ' ' '_' 117
 ++ loopLeft 121 (re " " tape)
 ++ goLeft 121 ' ' '.' 124
 ++ goLeft 124 ' ' '_' 117
 ++ loopLeft 122 (re " " tape)
 ++ goLeft 122 ' ' '1' 125
 ++ goLeft 125 ' ' '_' 117
 ++ loopLeft 126 (re "!" tape)
 ++ goRight 126 '!' '!' 127
 ++ loopRight 127 (re "y " tape)
 ++ goLeft 127 'y' '_' 128
 ++ goLeft 127 ' ' ',' 133
 ++ goRight 128 '0' '0' 129
 ++ goRight 128 '1' '1' 130
 ++ loopLeft 129 (re " " tape) 
 ++ goLeft 129 ' ' '0' 131
 ++ goLeft 131 ' ' '_' 126
 ++ loopLeft 130 (re " " tape)
 ++ goLeft 130 ' ' '1' 132
 ++ goLeft 132 ' ' '_' 126
 
 ++ loopLeft 133 (re "!" tape)
 ++ goRight 133 '!' '!' 134
 ++ loopRight 134 (re "z " tape)
 ++ goRight 134 ' ' ' ' 142
 ++ goLeft 134 'z' '_' 135
 ++ goRight 135 '0' '0' 136
 -- ++ 135 --> 137                                --missing
 ++ goRight 135 '.' '.' 137
 ++ goRight 135 '1' '1' 138
 ++ loopLeft 136 (re " " tape) 
 ++ goLeft 136 ' ' '0' 139
 ++ goLeft 139 ' ' '_' 133
 ++ loopLeft 137 (re " " tape)
 ++ goLeft 137 ' ' '.' 140
 ++ goLeft 140 ' ' '_' 133
 ++ loopLeft 138 (re " " tape)
 ++ goLeft 138 ' ' '1' 141
 ++ goLeft 141 ' ' '_' 133
  
 ++ loopLeft 142 (re "!" tape)
 ++ goRight 142 '!' '!' 143
 ++ loopRight 143 (re "w " tape)
 ++ goRight 143 ' ' ' ' 151      --next page
 ++ goLeft 143 'w' '_' 144
 ++ goRight 144 '0' '0' 145
 -- ++ 144 --> 146                                --missing
 ++ goRight 144 '.' '.' 146
 ++ goRight 144 '1' '1' 147
 ++ loopLeft 145 (re " " tape) 
 ++ goLeft 145 ' ' '0' 148
 ++ goLeft 148 ' ' '_' 142
 ++ loopLeft 146 (re " " tape)
 ++ goLeft 146 ' ' '.' 149
 ++ goLeft 149 ' ' '_' 142
 ++ loopLeft 147 (re " " tape)
 ++ goLeft 147 ' ' '1' 150
 ++ goLeft 150 ' ' '_' 142
 
 -- new page:
 ++ goLeft 151 ',' ',' 152
 ++ goRight 151 '_' '_' 163
 ++ loopLeft 152 (re "!" tape)
 ++ goRight 152 '!' '!' 153
 ++ loopRight1 153 't' "_"
 ++ loopRight 153 "_$"
 ++ goLeft 153 '$' '$' 154
 ++ loopLeft 154 "!"
 ++ goRight 154 '!' '!' 155
 ++ loopRight 155 (re "t$" tape)
 ++ goRight 155 '$' '$' 156
 ++ goLeft 155 't' '_' 158
 ++ goRight 158 '0' '0' 157
 ++ goRight 158 '.' '.' 159
 ++ loopRight 157 (re " " tape)
 ++ goLeft 157 ' ' '0' 191
 ++ goLeft 191 ' ' '_' 154
 ++ loopRight 159 (re " " tape)
 ++ goLeft 159 ' ' '.' 160
 ++ goLeft 160 ' ' '_' 154
 ++ loopRight 161 (re " " tape)
 ++ goLeft 161 ' ' '1' 162
 ++ goLeft 162 ' ' '_' 154
 ++ loopRight 156 (re " " tape)
 ++ goLeft 156 ' ' ' ' 164
 ++ goLeft 163 ' ' ' ' 164
 ++ loopLeft 164 (re "!t" tape)
 ++ loopLeft1 164 '_' "t"
 ++ goRight 164 '!' '!' 1
 
 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 







utm =
  TM [1 .. 191] ".01$;,:yutwvxz" tape id ' ' '!' transition 1 []
  -- where
    

      -- goRight 1 ' ' ' ' 6
        -- ++ loopRight 1 "*"
        -- ++ goRight 1 'a' '*' 2
        -- ++ loopRight 2 "a*"
        -- ++ goRight 2 'b' '*' 3
        -- ++ loopRight 3 "b*"
        -- ++ goRight 3 'c' '*' 4
        -- ++ loopRight 4 "c*"
        -- ++ goLeft 4 ' ' ' ' 5
        -- ++ loopLeft 5 "abc*"
        -- ++ goRight 5 '!' '!' 1





























