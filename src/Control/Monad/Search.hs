-- |
--
-- Module:      Control.Monad.Search
-- Description: Monad for backtracking
-- Stability:   experimental

module Control.Monad.Search
  ( MonadSearch(..)
  , DFS
  , dfs
  , BFS
  , bfs
  )
where

import           Data.Foldable                  ( toList )
import           Control.Applicative            ( Alternative(..) )
import           Control.Monad                  ( ap, MonadPlus(..) )

-- | 'MonadSearch' represents searches with backtracking.
class MonadSearch m where
  fromList :: [a] -> m a
  toList   :: m a -> [a]

-- | DFS implementation of 'MonadSearch'.
newtype DFS a = DFS { unDFS :: [a] }
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadSearch DFS where
  fromList xs = DFS xs
  toList (DFS xs) = xs

dfs :: a -> DFS a
dfs x = DFS [x]

-- | BFS implementation of 'MonadSearch'.
newtype BFS a = BFS { unBFS :: [[a]] }

instance Functor BFS where
  fmap f (BFS xss) = BFS $ map (\xs -> map f xs) xss

instance Applicative BFS where
  pure x = BFS [[x]]
  (<*>) = ap

instance Monad BFS where
  return = pure
  BFS [] >>= _ = BFS []
  BFS (xs:xss) >>= f = foldl mplus mzero (map f xs) `mplus` shift (BFS xss >>= f)
   where
    shift :: BFS a -> BFS a
    shift (BFS xss) = BFS ([] : xss)

instance Alternative BFS where
  empty = BFS []
  (<|>) (BFS xss) (BFS yss) = BFS (merge xss yss)
   where
    merge :: [[a]] -> [[a]] -> [[a]]
    merge [] [] = []
    merge xss [] = xss
    merge [] yss = yss
    merge (xs:xss) (ys:yss) = (xs ++ ys) : merge xss yss

instance MonadPlus BFS where
  mzero = empty
  mplus = (<|>)

instance MonadSearch BFS where
  fromList xs = BFS (map (\x -> [x]) xs)
  toList (BFS xss) = concat xss

bfs :: a -> BFS a
bfs x = BFS [[x]]
