module TheOneTM where

import Data.Char
import Data.List
import TM
import Util

uTape :: String
uTape = " !DACLR;:#_uvwxyz"

-- encode base on its order in the input tape
-- offset is 1 when encoding state
class UEncode a where
  wzenc :: a -> String -- over utape

--states
instance UEncode Int where
  wzenc x = "D_" ++ concat (replicate x "A_")

instance UEncode Integer where
  wzenc x = wzenc (fromIntegral x :: Int)

-- tape
-- https://stackoverflow.com/questions/19545253/haskell-replace-characters-in-string
instance UEncode Char where
  wzenc x =
    let tmp = (wzenc $ ord x)
     in map (\c -> if c == 'A' then 'C' else c) tmp

instance UEncode Direction where
  wzenc GoLeft = "L_"
  wzenc GoRight = "R_"

{- convenience function to encode a, and then append another string.
    The Char u separates thetwo strings.
-}
wzencSep :: UEncode a => String -> a -> String -> String
wzencSep u x rest = wzenc x ++ u ++ rest

colon :: UEncode a => a -> String -> String
colon = wzencSep ":"

pound :: UEncode a => a -> String -> String
pound = wzencSep "#"

semiColon :: UEncode a => a -> String -> String
semiColon = wzencSep ";"

instance (UEncode state, UEncode tape) => UEncode (Trans state tape) where
  wzenc (Trans st g d st' g') =
    wzenc st
      ++ wzenc g
      ++ wzenc g'
      ++ wzenc d
      ++ wzenc st'

-- https://stackoverflow.com/questions/9220986/is-there-any-haskell-function-to-concatenate-list-with-separator
listH :: UEncode a => [a] -> String -> String
listH xs rest = concatMap (\a -> ";_" ++ wzenc a) xs ++ rest

-- assume 1 final state
encodeh ::
  (UEncode input, UEncode state, UEncode tape) =>
  TM input state tape ->
  String {- string to follow this one -} ->
  String -- over Utape
encodeh (TM states inputs tapesyms _ blank leftend trans start final) rest =
  pound leftend $
    pound blank $
      (wzenc (head final) ++) $
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
inputU tm xs = encodeh tm ""