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
class (Foldable m, MonadPlus m) => MonadSearch m where
  wrap    :: [a] -> m a
  collect :: m a -> [a]

  default collect :: m a -> [a]
  collect = toList

-- | DFS implementation of 'MonadSearch'.
newtype DFS a = DFS { unDFS :: [a] }
  deriving newtype (Functor, Applicative, Monad, Foldable, Alternative, MonadPlus)

instance MonadSearch DFS where
  wrap xs = DFS xs

dfs :: a -> DFS a
dfs x = DFS [x]

-- | BFS implementation of 'MonadSearch'.
newtype BFS a = BFS { unBFS :: [[a]] }

instance Show (BFS a) where
  show (BFS []) = "[]"
  show (BFS (xs:xss)) = show (length xs) ++ " : " ++ show (BFS xss)

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

instance Foldable BFS where
  foldr _ z (BFS []) = z
  foldr f z (BFS (xs:xss)) = foldr f (foldr f z (BFS xss)) xs

instance MonadSearch BFS where
  wrap xs = BFS (map (\x -> [x]) xs)

bfs :: a -> BFS a
bfs x = BFS [[x]]
