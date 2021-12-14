module TheOneTM where

import Data.Char
import Data.List
import TM
import TMExamples
import Util

uTape :: String
uTape = " !;:$,._tuvwxyz10"

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

-- pound :: UEncode a => a -> String -> String
-- pound = bitencPre "#"

-- poundDot :: UEncode a => a -> String -> String
-- poundDot = bitencPre "#._"

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

--------------------------------------------------------------------------------
-- remove elements from the list
r :: Eq a => a -> [a] -> [a]
r _ [] = []
r x (y : ys)
  | x == y = r x ys
  | otherwise = y : r x ys

-- remove list of chars (strings) from the tape symbols
re :: Eq a => [a] -> [a] -> [a]
re [] tape = tape
re (x : xs) tape = re xs (r x tape)

-- 1st tape is the new tape symbol, the 2nd arg - [tape] is the list of old symbols
-- need to make a new loop function as the previous loop fxn cannot change tape symbols
loop1 :: Direction -> state -> tape -> [tape] -> [Trans state tape]
loop1 d st newtape = (map (\g -> Trans st g d st newtape))

loopRight1 :: state -> tape -> [tape] -> [Trans state tape]
loopRight1 = loop1 GoRight

loopLeft1 :: state -> tape -> [tape] -> [Trans state tape]
loopLeft1 = loop1 GoLeft

tape = ".01$;,:yutwvxz! _"

inputSym = ".01$;,:yutwvxz_"

transition =
  -- mark current state and sym being scanned by TM with y
  loopRight 1 (re " " tape)
    ++ goLeft 1 ' ' ' ' 2
    ++ loopLeft 2 (re "," tape) -- loop without changing symbols -- loop
    ++ goLeft 2 ',' ',' 3
    ++ loopLeft1 3 'y' "_" -- loop with changing symbols -- loop 1
    ++ loopLeft 3 (re "_," tape)
    ++ goLeft 3 ',' ',' 4
    ++ loopLeft1 4 'y' "_"
    ++ loopLeft 4 (re "_," tape)
    ++ goLeft 4 ',' ',' 5
    ++ loopLeft 5 (re "!" tape)
    ++ goRight 5 '!' '!' 6
    -- find and mark next unexplored transition (without z) with y
    ++ loopRight 6 (re "; " tape)
    ++ goRight 6 ';' ';' 190
    ++ goLeft 6 ' ' ' ' 165 -- TO THE ACCEPT PATH
    ++ goRight 190 'z' 'z' 6
    ++ goRight 190 '_' '_' 7
    ++ loopRight 7 (re "_," tape)
    ++ loopRight1 7 'x' "_"
    ++ goRight 7 ',' ',' 8
    ++ loopRight 8 (re "_," tape)
    ++ loopRight1 8 'x' "_"
    --compare to see if this transition matches the current state and sym
    ++ goRight 8 ',' ',' 9
    ++ loopLeft 9 (re "!" tape)
    ++ goRight 9 '!' '!' 10
    ++ loopRight 10 (re " y" tape)
    ++ goLeft 10 ' ' ' ' 21
    ++ goLeft 10 'y' '_' 11
    ++ goLeft 11 '0' '0' 12
    ++ goLeft 11 '.' '.' 13
    ++ goLeft 11 '1' '1' 14
    -- left branch
    ++ loopLeft 12 (re "!" tape)
    ++ goRight 12 '!' '!' 15
    ++ loopRight 15 (re " x" tape)
    ++ goLeft 15 'x' '_' 18
    ++ goLeft 15 ' ' ' ' 23
    ++ goLeft 18 '.' '.' 22
    ++ goLeft 18 '1' '1' 22
    ++ goLeft 18 '0' '0' 9
    -- middle branch
    ++ loopLeft 13 (re "!" tape)
    ++ goRight 13 '!' '!' 16
    ++ loopRight 16 (re " x" tape)
    ++ goLeft 16 ' ' ' ' 23
    ++ goLeft 16 'x' '_' 19
    ++ goLeft 19 '0' '0' 22
    ++ goLeft 19 '1' '1' 22
    ++ goLeft 19 '.' '.' 9
    -- right branch
    ++ loopLeft 14 (re "!" tape)
    ++ goRight 14 '!' '!' 17
    ++ loopRight 17 (re " x" tape)
    ++ goLeft 17 ' ' ' ' 23
    ++ goLeft 17 'x' '_' 20
    ++ goLeft 20 '1' '1' 9
    ++ goLeft 20 '.' '.' 22
    ++ goLeft 20 '0' '0' 22
    -- compare fail, erase x, y, mark this transition with z
    ++ loopLeft 21 (re "!x" tape)
    ++ goLeft 21 'x' '_' 22
    ++ loopRight 22 (re " " tape)
    ++ goLeft 22 ' ' ' ' 23
    ++ loopLeft1 23 '_' "xy"
    ++ loopLeft 23 (re "xy!" tape)
    ++ goRight 23 '!' '!' 24
    ++ loopRight 24 (re ";" tape)
    ++ goRight 24 ';' ';' 25
    ++ goRight 25 'z' 'z' 24
    ++ goRight 25 '_' 'z' 1
    --compare suceed:
    -- go to the matched transition
    ++ goRight 21 '!' '!' 26
    ++ loopRight 26 (re ";" tape)
    ++ goRight 26 ';' ';' 27
    ++ goRight 27 'z' 'z' 26
    ++ goRight 27 '_' '_' 192
    ++ loopRight 192 (re "," tape)
    ++ goRight 192 ',' ',' 28
    ++ loopRight 28 (re "," tape)
    ++ goRight 28 ',' ',' 29
    -- mark new sym, dir with u, mark new state with y, erase z
    ++ loopRight1 29 'u' "_"
    ++ loopRight 29 (re "_," tape)
    ++ goRight 29 ',' ',' 30
    ++ loopRight1 30 'u' "_"
    ++ loopRight 30 (re "_," tape)
    ++ goRight 30 ',' ',' 31
    ++ loopRight1 31 'y' "_"
    ++ loopRight 31 (re "_;:" tape)
    ++ goLeft 31 ';' ';' 32
    ++ goLeft 31 ':' ':' 32
    ++ loopLeft 32 (re "z!" tape)
    ++ loopLeft1 32 '_' "z"
    ++ goRight 32 '!' '!' 33
    ++ loopRight 33 (re " " tape)
    ++ goLeft 33 ' ' ' ' 34
    -- go to current config, mark left part with v, mark previous symbol with x, mark next symbol with z, mark the right part with w
    ++ loopLeft 34 (re ":" tape)
    ++ goRight 34 ':' ':' 35
    ++ goRight 35 '_' '_' 36
    ++ loopRight 36 (re ",_" tape)
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
    ++ loopLeft 45 (re "u" tape)
    ++ goLeft 45 'u' '_' 46
    -- update config - PqSQ
    ++ goRight 46 '1' '1' 47
    ++ goRight 46 '0' '0' 99
    -- top branch: go left
    ++ loopRight 47 (re "v " tape)
    ++ goLeft 47 ' ' ',' 56
    ++ goLeft 47 'v' '_' 48
    ++ goRight 48 '0' '0' 49
    ++ goRight 48 '.' '.' 50
    ++ goRight 48 '1' '1' 51
    ++ loopRight 49 (re " " tape)
    ++ goRight 49 ' ' '0' 52
    ++ goLeft 52 ' ' '_' 55
    ++ loopRight 50 (re " " tape)
    ++ goRight 50 ' ' '.' 53
    ++ goLeft 53 ' ' '_' 55
    ++ loopRight 51 (re " " tape)
    ++ goRight 51 ' ' '1' 54
    ++ goLeft 54 ' ' '_' 55
    ++ loopLeft 55 (re "!" tape)
    ++ goRight 55 '!' '!' 47
    ++ loopLeft 56 (re "!" tape)
    ++ goRight 56 '!' '!' 57
    ++ loopRight 57 (re "y " tape)
    ++ goLeft 57 'y' '_' 58
    ++ goLeft 57 ' ' ',' 63
    ++ goRight 58 '0' '0' 59
    ++ goRight 58 '1' '1' 60
    ++ loopRight 59 (re " " tape)
    ++ goRight 59 ' ' '0' 61
    ++ goLeft 61 ' ' '_' 56
    ++ loopRight 60 (re " " tape)
    ++ goRight 60 ' ' '1' 62
    ++ goLeft 62 ' ' '_' 56
    ++ loopLeft 63 (re "!" tape)
    ++ goRight 63 '!' '!' 64
    ++ loopRight 64 (re "x " tape)
    ++ goLeft 64 ' ' ',' 72
    ++ goLeft 64 'x' '_' 65
    ++ goRight 65 '0' '0' 66
    ++ goRight 65 '.' '.' 67
    ++ goRight 65 '1' '1' 68
    ++ loopRight 66 (re " " tape)
    ++ goRight 66 ' ' '0' 69
    ++ goLeft 69 ' ' '_' 63
    ++ loopRight 67 (re " " tape)
    ++ goRight 67 ' ' '.' 70
    ++ goLeft 70 ' ' '_' 63
    ++ loopRight 68 (re " " tape)
    ++ goRight 68 ' ' '1' 71
    ++ goLeft 71 ' ' '_' 63
    ++ loopLeft 72 (re "!" tape)
    ++ goRight 72 '!' '!' 73
    ++ loopRight 73 (re "u " tape)
    ++ goLeft 73 'u' '_' 74
    ++ goLeft 73 ' ' ' ' 81
    ++ goRight 74 '0' '0' 75
    ++ goRight 74 '.' '.' 76
    ++ goRight 74 '1' '1' 77
    ++ loopRight 75 (re " " tape)
    ++ goRight 75 ' ' '0' 78
    ++ goLeft 78 ' ' '_' 72
    ++ loopRight 76 (re " " tape)
    ++ goRight 76 ' ' '.' 79
    ++ goLeft 79 ' ' '_' 72
    ++ loopRight 77 (re " " tape)
    ++ goRight 77 ' ' '1' 80
    ++ goLeft 80 ' ' '_' 72
    ++ loopLeft 81 (re "!" tape)
    ++ goRight 81 '!' '!' 82
    ++ loopRight 82 (re "z " tape)
    ++ goLeft 82 ' ' ' ' 90
    ++ goLeft 82 'z' '_' 83
    ++ goRight 83 '0' '0' 84
    ++ goRight 83 '.' '.' 85
    ++ goRight 83 '1' '1' 86
    ++ loopRight 84 (re " " tape)
    ++ goRight 84 ' ' '0' 87
    ++ goLeft 87 ' ' '_' 81
    ++ loopRight 85 (re " " tape)
    ++ goRight 85 ' ' '.' 88
    ++ goLeft 88 ' ' '_' 81
    ++ loopRight 86 (re " " tape)
    ++ goRight 86 ' ' '1' 89
    ++ goLeft 89 ' ' '_' 81
    ++ loopLeft 90 (re "!" tape)
    ++ goRight 90 '!' '!' 91
    ++ loopRight 91 (re "w " tape)
    ++ goLeft 91 ' ' ' ' 164 --next page, done pasting
    ++ goLeft 91 'w' '_' 92
    ++ goRight 92 '0' '0' 93
    ++ goRight 92 '.' '.' 94
    ++ goRight 92 '1' '1' 95
    ++ loopRight 93 (re " " tape)
    ++ goRight 93 ' ' '0' 96
    ++ goLeft 96 ' ' '_' 90
    ++ loopRight 94 (re " " tape)
    ++ goRight 94 ' ' '.' 97
    ++ goLeft 97 ' ' '_' 90
    ++ loopRight 95 (re " " tape)
    ++ goRight 95 ' ' '1' 98
    ++ goLeft 98 ' ' '_' 90
    -- lower branch: goRight
    ++ loopRight 99 (re "v " tape)
    ++ goLeft 99 ' ' ' ' 108
    ++ goLeft 99 'v' '_' 100
    ++ goRight 100 '0' '0' 101
    ++ goRight 100 '.' '.' 102
    ++ goRight 100 '1' '1' 103
    ++ loopRight 101 (re " " tape)
    ++ goRight 101 ' ' '0' 104
    ++ goLeft 104 ' ' '_' 107
    ++ loopRight 102 (re " " tape)
    ++ goRight 102 ' ' '.' 105
    ++ goLeft 105 ' ' '_' 107
    ++ loopRight 103 (re " " tape)
    ++ goRight 103 ' ' '1' 106
    ++ goLeft 106 ' ' '_' 107
    ++ loopLeft 107 (re "!" tape)
    ++ goRight 107 '!' '!' 99
    ++ loopLeft 108 (re "!" tape)
    ++ goRight 108 '!' '!' 109
    ++ loopRight 109 (re "x " tape)
    ++ goLeft 109 'x' '_' 110
    ++ goLeft 109 ' ' ' ' 117
    ++ goRight 110 '0' '0' 111
    ++ goRight 110 '.' '.' 112
    ++ goRight 110 '1' '1' 113
    ++ loopRight 111 (re " " tape)
    ++ goRight 111 ' ' '0' 114
    ++ goLeft 114 ' ' '_' 108
    ++ loopRight 112 (re " " tape)
    ++ goRight 112 ' ' '.' 115
    ++ goLeft 115 ' ' '_' 108
    ++ loopRight 113 (re " " tape)
    ++ goRight 113 ' ' '1' 116
    ++ goLeft 116 ' ' '_' 108
    ++ loopLeft 117 (re "!" tape)
    ++ goRight 117 '!' '!' 118
    ++ loopRight 118 (re "u " tape)
    ++ goLeft 118 ' ' ',' 126
    ++ goLeft 118 'u' '_' 119
    ++ goRight 119 '0' '0' 120
    ++ goRight 119 '.' '.' 121
    ++ goRight 119 '1' '1' 122
    ++ loopRight 120 (re " " tape)
    ++ goRight 120 ' ' '0' 123
    ++ goLeft 123 ' ' '_' 117
    ++ loopRight 121 (re " " tape)
    ++ goRight 121 ' ' '.' 124
    ++ goLeft 124 ' ' '_' 117
    ++ loopRight 122 (re " " tape)
    ++ goRight 122 ' ' '1' 125
    ++ goLeft 125 ' ' '_' 117
    ++ loopLeft 126 (re "!" tape)
    ++ goRight 126 '!' '!' 127
    ++ loopRight 127 (re "y " tape)
    ++ goLeft 127 'y' '_' 128
    ++ goLeft 127 ' ' ',' 133
    ++ goRight 128 '0' '0' 129
    ++ goRight 128 '1' '1' 130
    ++ loopRight 129 (re " " tape)
    ++ goRight 129 ' ' '0' 131
    ++ goLeft 131 ' ' '_' 126
    ++ loopRight 130 (re " " tape)
    ++ goRight 130 ' ' '1' 132
    ++ goLeft 132 ' ' '_' 126
    ++ loopLeft 133 (re "!" tape)
    ++ goRight 133 '!' '!' 134
    ++ loopRight 134 (re "z " tape)
    ++ goLeft 134 ' ' ',' 142
    ++ goLeft 134 'z' '_' 135
    ++ goRight 135 '0' '0' 136
    ++ goRight 135 '.' '.' 137
    ++ goRight 135 '1' '1' 138
    ++ loopRight 136 (re " " tape)
    ++ goRight 136 ' ' '0' 139
    ++ goLeft 139 ' ' '_' 133
    ++ loopRight 137 (re " " tape)
    ++ goRight 137 ' ' '.' 140
    ++ goLeft 140 ' ' '_' 133
    ++ loopRight 138 (re " " tape)
    ++ goRight 138 ' ' '1' 141
    ++ goLeft 141 ' ' '_' 133
    ++ loopLeft 142 (re "!" tape)
    ++ goRight 142 '!' '!' 143
    ++ loopRight 143 (re "w " tape)
    ++ goLeft 143 ' ' ' ' 151 --next page
    ++ goLeft 143 'w' '_' 144
    ++ goRight 144 '0' '0' 145
    ++ goRight 144 '.' '.' 146
    ++ goRight 144 '1' '1' 147
    ++ loopRight 145 (re " " tape)
    ++ goRight 145 ' ' '0' 148
    ++ goLeft 148 ' ' '_' 142
    ++ loopRight 146 (re " " tape)
    ++ goRight 146 ' ' '.' 149
    ++ goLeft 149 ' ' '_' 142
    ++ loopRight 147 (re " " tape)
    ++ goRight 147 ' ' '1' 150
    ++ goLeft 150 ' ' '_' 142
    -- new page:
    ++ goLeft 151 ',' ',' 152
    ++ goRight 151 '_' '_' 163
    ++ loopLeft 152 (re "!" tape)
    ++ goRight 152 '!' '!' 153
    ++ loopRight1 153 't' "_"
    ++ loopRight 153 (re "_$" tape)
    ++ goLeft 153 '$' '$' 154
    ++ loopLeft 154 (re "!" tape)
    ++ goRight 154 '!' '!' 155
    ++ loopRight 155 (re "t$" tape)
    ++ goRight 155 '$' '$' 156
    ++ goLeft 155 't' '_' 158
    ++ goRight 158 '0' '0' 157
    ++ goRight 158 '.' '.' 159
    ++ goRight 158 '1' '1' 161
    ++ loopRight 157 (re " " tape)
    ++ goRight 157 ' ' '0' 191
    ++ goLeft 191 ' ' '_' 154
    ++ loopRight 159 (re " " tape)
    ++ goRight 159 ' ' '.' 160
    ++ goLeft 160 ' ' '_' 154
    ++ loopRight 161 (re " " tape)
    ++ goRight 161 ' ' '1' 162
    ++ goLeft 162 ' ' '_' 154
    ++ loopRight 156 (re " " tape)
    ++ goLeft 156 ' ' ' ' 164
    ++ goLeft 163 ' ' ' ' 164
    ++ loopLeft 164 (re "!t" tape)
    ++ loopLeft1 164 '_' "t"
    ++ goRight 164 '!' '!' 1
    -- check final
    ++ loopLeft 165 (re "," tape)
    ++ goLeft 165 ',' ',' 166
    ++ loopLeft 166 (re "," tape)
    ++ goLeft 166 ',' ',' 167
    ++ loopLeft 167 (re "," tape)
    ++ goRight 167 ',' ',' 168
    ++ loopRight 168 (re "y,_" tape)
    ++ loopRight1 168 't' "y_"
    ++ goRight 168 ',' ',' 169
    ++ loopLeft 169 (re "!" tape)
    ++ goRight 169 '!' '!' 170
    ++ loopRight 170 (re "$;:" tape)
    ++ goRight 170 ':' ':' 171
    ++ goRight 170 ';' ';' 171 -- reject
    ++ goRight 170 '$' '$' 172
    ++ goRight 172 'w' 'w' 170
    ++ goRight 172 '_' '_' 173
    ++ loopRight1 173 'u' "_"
    ++ loopRight 173 (re "_$;:" tape)
    ++ goLeft 173 '$' '$' 174
    ++ goLeft 173 ';' ';' 174
    ++ goLeft 173 ':' ':' 174
    ++ loopLeft 174 (re "!" tape)
    ++ goRight 174 '!' '!' 175
    ++ loopRight 175 (re " t" tape)
    ++ goLeft 175 't' '_' 176
    ++ goLeft 175 ' ' ' ' 184
    ++ goLeft 176 '1' '1' 177
    ++ goLeft 176 '0' '0' 178
    ++ loopLeft 177 (re "!" tape)
    ++ goRight 177 '!' '!' 179
    ++ loopLeft 178 (re "!" tape)
    ++ goRight 178 '!' '!' 180
    ++ loopRight 179 (re " u" tape)
    ++ goLeft 179 'u' '_' 181
    ++ goLeft 179 ' ' ' ' 186
    ++ loopRight 180 (re " u" tape)
    ++ goLeft 180 'u' '_' 182
    ++ goLeft 180 ' ' ' ' 186
    ++ goLeft 181 '1' '1' 174
    ++ goLeft 181 '0' '0' 183
    ++ goLeft 182 '1' '1' 183
    ++ goLeft 182 '0' '0' 174
    ++ loopRight 183 (re " " tape)
    ++ goLeft 183 ' ' ' ' 186
    ++ loopLeft 184 (re "!u" tape)
    ++ goRight 184 '!' '!' 185 -- accept
    ++ goLeft 184 'u' '_' 183
    ++ loopLeft 186 (re "ut!" tape)
    ++ loopLeft1 186 '_' "ut"
    ++ goRight 186 '!' '!' 187
    ++ loopRight 187 (re "$" tape)
    ++ goRight 187 '$' '$' 188
    ++ goRight 188 'w' 'w' 187
    ++ goRight 188 '_' 'w' 189
    ++ loopRight 189 (re " " tape)
    ++ goLeft 189 ' ' ' ' 165

utm =
  TM [1 .. 192] inputSym tape id ' ' '!' transition 1 [185]
