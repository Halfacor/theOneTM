module TheOneTM where

import Data.Char
import Data.List
import TM
import Util

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
