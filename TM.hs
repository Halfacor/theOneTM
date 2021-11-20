module TM where

import Data.List
import GvShow
import Util

data Direction = GoLeft | GoRight
  deriving Show

data Trans state tape = Trans state tape Direction state tape 
  deriving Show 

data TM input state tape = TM {
  states :: [state], -- Q: all states.  
  inputs :: [input], -- Sigma: all possible inputs.
  tapesyms :: [tape], -- Gamma: all possible stack symbols.
  totape :: input -> tape, -- witnesses the inclusion of Sigma into Gamma
  blank :: tape, -- blank symbol
  leftend :: tape, -- left endmarker
  trans :: [Trans state tape], -- R: transition relation
  start :: state, -- start state
  final :: [state]} -- final states

data Config state tape = Config state tape {- the blank -} [tape] [tape]

instance (Show state, Show tape, Eq tape) => Show (Config state tape) where
  show (Config st blank leftrev right) =
    "[" ++ show st ++ ": " ++ (show $ reverse leftrev) ++ " " ++ (show $ takeWhile (/= blank) right) ++ "]"

shiftConfig :: Config state tape -> Direction -> Config state tape
shiftConfig (Config st blank leftrev (r:right)) GoRight = Config st blank (r:leftrev) right
shiftConfig (Config st blank (l:leftrev) (right)) GoLeft = Config st blank leftrev (l:right)

updateConfig :: Config state tape -> state -> tape -> Direction -> Config state tape 
updateConfig (Config st blank (l:leftrev) right) st' g = shiftConfig (Config st' blank (g:leftrev) right)

newConfigs :: (Eq state, Eq tape) => TM input state tape -> Config state tape -> [Config state tape]
newConfigs tm c@(Config st _ (l:leftrev) (right)) =
  let nexts = [ (st'',g'',d) | Trans st' g' d st'' g'' <- (trans tm) , st == st' , l == g' ] in
    let newConfig (st,g,d) = updateConfig c st g d in
      map newConfig nexts

-- breadth-first search for accepting config
acceptsh :: (Eq state, Eq tape) => TM input state tape -> [Config state tape] -> Bool
acceptsh tm [] = False
acceptsh tm (c@(Config st _ _ _) : cs) =
  if elem st (final tm) then True
  else
    acceptsh tm (cs ++ newConfigs tm c)

initialConfig :: TM input state tape -> [input] -> Config state tape
initialConfig tm xs =
  let t = map (totape tm) xs ++ repeat (blank tm) in
    Config (start tm) (blank tm) [head t , leftend tm] (tail t)

accepts :: (Eq state, Eq tape) => TM input state tape -> [input] -> Bool
accepts tm xs = acceptsh tm [initialConfig tm xs]

epsEdge :: state -> state -> state -> [tape] -> [Trans state tape]
epsEdge p intermediate q ts =
  concat (do
             t <- ts
             return [Trans p t GoRight intermediate t, Trans intermediate t GoLeft q t])

goRight :: state -> tape -> tape -> state -> [Trans state tape]
goRight st g g' st' = [Trans st g GoRight st' g']

goLeft :: state -> tape -> tape -> state -> [Trans state tape]
goLeft st g g' st' = [Trans st g GoLeft st' g']

loop :: Direction -> state -> [tape] -> [Trans state tape]
loop d st = map (\ g -> Trans st g d st g) 

loopRight :: state -> [tape] -> [Trans state tape]
loopRight = loop GoRight

loopLeft :: state -> [tape] -> [Trans state tape]
loopLeft = loop GoLeft

toGraphViz :: (GvShow input, GvShow state, Eq state, GvShow tape) => TM input state tape -> String
toGraphViz (TM states inputs _ _ _ _ trans start finals) =
    "digraph pda {\n" ++
    "graph [pad=\"1\", nodesep=\".5\", ranksep=\"1\"];\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    "node [shape = doublecircle];\n" ++
    (foldrGlue (\ f str -> 
                   gvshow f ++ " [label = \"\"];\n" ++ str)
               finals
       ("node [shape = point];\n" ++
        "hidden -> " ++ gvshow start ++ ";\n" ++ 
       -- loop over transitions st
       (foldrGlue (\ (Trans st g d st' g') str ->
                     gvshow st ++ " -> " ++ gvshow st' ++ " [label = \"" ++ gvshow g ++ (case d of { GoRight -> "/" ; GoLeft -> "\\\\"})
                     ++ gvshow g' ++ "\"];\n" ++ str)
         trans
         "}\n")))

-- write the given NFA in GraphViz format to the file named filename.
writeGraphViz :: (GvShow input, GvShow state, Eq state, GvShow tape) => String -> TM input state tape -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz d)
