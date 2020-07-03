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

import           Control.Applicative            ( Alternative(..) )
import           Control.Monad                  ( ap, MonadPlus(..) )

-- | 'MonadSearch' represents searches with backtracking.
class MonadPlus m => MonadSearch m where
  fromList :: [a] -> m a
  toList   :: m a -> [a]

  failure  :: m a -> Bool
  default failure :: m a -> Bool
  failure m = null (toList m)
  lnot     :: DFS a -> m ()
  default lnot :: DFS a -> m ()
  lnot m = if failure m then fromList [()] else mzero
  guard    :: Bool -> m ()
  default guard :: Bool -> m ()
  guard t = if t then fromList [()] else mzero

-- | DFS implementation of 'MonadSearch'.
newtype DFS a = DFS { unDFS :: [a] }
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus)

instance MonadSearch DFS where
  {-# INLINE fromList #-}
  fromList xs = DFS xs
  {-# INLINE toList #-}
  toList (DFS xs) = xs

dfs :: a -> DFS a
dfs x = DFS [x]

-- | BFS implementation of 'MonadSearch'.
newtype BFS a = BFS { unBFS :: [[a]] }

instance Functor BFS where
  fmap f (BFS xss) = BFS $ map (\xs -> map f xs) xss

instance Applicative BFS where
  pure = return
  (<*>) = ap

instance Monad BFS where
  return x = BFS [[x]]
  BFS [] >>= _ = BFS []
  BFS (xs:xss) >>= f = foldl mplus mzero (map f xs) `mplus` shift (BFS xss >>= f)
   where
    shift :: BFS a -> BFS a
    shift (BFS xss) = BFS ([] : xss)

instance Alternative BFS where
  empty = mzero
  (<|>) = mplus

instance MonadPlus BFS where
  mzero = BFS []
  mplus (BFS xss) (BFS yss) = BFS (merge xss yss)
   where
    merge :: [[a]] -> [[a]] -> [[a]]
    merge [] [] = []
    merge xss [] = xss
    merge [] yss = yss
    merge (xs:xss) (ys:yss) = (xs ++ ys) : merge xss yss

instance MonadSearch BFS where
  fromList xs = BFS (map (\x -> [x]) xs)
  toList (BFS xss) = concat xss

bfs :: a -> BFS a
bfs x = BFS [[x]]
