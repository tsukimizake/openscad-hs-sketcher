module UnionFind (UnionFind, emptyUF, find, union) where

import qualified Data.Map as Map
import SketchTypes

emptyUF :: UnionFind
emptyUF = UnionFind Map.empty

find :: Id -> UnionFind -> Id
find x uf = case Map.lookup x uf.parent of
  Just p | p /= x -> find p uf
  _ -> x

union :: Id -> Id -> UnionFind -> UnionFind
union x y uf =
  let px = find x uf
      py = find y uf
   in if px == py
        then uf
        else
          UnionFind $ Map.insert py px uf.parent -- left arg is the new parent
