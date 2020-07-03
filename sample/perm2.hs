import           Control.Monad.Search
import           Data.List
import           Data.Maybe
import           System.Environment


perm2 :: [a] -> [(a, a)]
perm2 xs = toList $ dfs xs >>= fromList . tails >>= (\xs' -> fromList (maybeToList (uncons xs'))) >>= \(x, ys) -> fromList (tails ys) >>= (\ys' -> fromList (maybeToList (uncons ys))) >>= \(y, _) -> pure (x, y)

main = do
  [n] <- getArgs
  let n' = read n :: Int
  print $ length $ perm2 [1..n']
