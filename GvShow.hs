module GvShow where

{- this type class is a version of Show for showing things in a
   format that Graphviz accepts -}
class GvShow a where
  gvshow :: a -> String

instance (GvShow a, GvShow b) => GvShow (a,b) where
  gvshow (a, b) = gvshow a ++ "_" ++ gvshow b

-- print lists out differently for benefit of rendering to graphviz
instance GvShow a => GvShow [a] where
  gvshow xs = "l_" ++ foldr (\ a str -> gvshow a ++ "_" ++ str) "e" xs

instance GvShow Char where
  gvshow = show

instance GvShow Int where
  gvshow = show

instance GvShow Integer where
  gvshow = show