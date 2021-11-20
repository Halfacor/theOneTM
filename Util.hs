module Util where

import Data.Tree
import Data.List

-- canonize a list of ordered elements
canonOrd :: Ord a => [a] -> [a] 
canonOrd = sort . nub

fixedPoint :: Eq a => a -> (a -> a) -> a
fixedPoint x f =
  let x' = f x in
    if x == x' then
      x'
    else
      fixedPoint x' f

--fixedPoint2 :: [a] -> ([a] -> [a]) -> [a]
--fixedPoint2 x f = concat $ iterate f x

foldrGlue f xs str = foldr f str xs

promote :: String -> [String]
promote = map (\ s -> [s])

type Path = [Int]
type Context a = Tree a -> Tree a

{- assumes the path is legal for the tree -}
subtreeAt :: Path -> Tree a -> Tree a
subtreeAt [] t = t
subtreeAt (i:ii) (Node x ts) = subtreeAt ii (ts !! i)

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] _ _ = []
replaceElem (x : xs) 0 x' = x' : xs
replaceElem (x : xs) n x' = x : replaceElem xs (n - 1) x'

{- path should be legal for the tree -}
contextTo :: Path -> Tree a -> Context a
contextTo [] t = id
contextTo (i:ii) (Node x ts) =
  let c = \ n -> Node x (replaceElem ts i n) 
      c' = contextTo ii (ts !! i) in
    c . c'

iter :: Int -> (a -> a) -> a -> a
iter 0 f a = a
iter n f a = f (iter (n - 1) f a)