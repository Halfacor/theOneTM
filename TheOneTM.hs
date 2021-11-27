module TheOneTM where

import Data.Char
import Data.List
import TM
import Util

uTape :: String
uTape = " !DACLR;:#,._uvwxyz10"

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
bitencSep :: UEncode a => String -> a -> String -> String
bitencSep u x rest = bitenc x ++ (u ++ rest)

colon :: UEncode a => a -> String -> String
colon = bitencSep ":_"

comma :: UEncode a => a -> String -> String
comma = bitencSep ","

dot :: UEncode a => a -> String -> String
dot = bitencSep "."

pound :: UEncode a => a -> String -> String
pound = bitencSep "#"

semiColon :: UEncode a => a -> String -> String
semiColon = bitencSep ";_"

instance (UEncode state, UEncode tape) => UEncode (Trans state tape) where
  bitenc (Trans st g d st' g') =
    dot st $
      dot g $
        dot g' $
          dot d $
            dot st' ""

-- https://stackoverflow.com/questions/9220986/is-there-any-haskell-function-to-concatenate-list-with-separator
listH :: UEncode a => [a] -> String -> String
listH xs rest = concatMap (\a -> ";_" ++ bitenc a) xs ++ rest

-- assume only 1 final state (add eps )
encodeh ::
  (UEncode input, UEncode state, UEncode tape) =>
  TM input state tape ->
  String {- string to follow this one -} ->
  String -- over Utape
encodeh (TM states inputs tapesyms _ blank leftend trans start final) rest =
  pound leftend $
    pound blank $
      pound start $
        (intercalate "," (map (\f -> bitenc f ++ "") final) ++) $
          listH trans rest

encode ::
  (UEncode input, UEncode state, UEncode tape) =>
  TM input state tape ->
  String -- over Utape
encode tm = encodeh tm ""

-- turn a TM and an input string into a single input for the universal TM
inputU ::
  (UEncode input, UEncode state, UEncode tape) =>
  TM input state tape ->
  [input] ->
  String -- over Utape
inputU tm xs = encodeh tm (":_:_!_," ++ bitenc (start tm) ++ "," ++ tmp)
  where
    tmp = case length xs of
      1 -> bitenc (head xs) ++ ".," ++ bitenc (blank tm)
      0 -> bitenc (blank tm) ++ ".," ++ bitenc (blank tm)
      _ -> bitenc (head xs) ++ ".," ++ concatMap (\x -> bitenc x ++ ".") (tail xs)

--------------------------------------------------------------------------------
