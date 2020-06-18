module Spec
  (
    test_dfs
  , test_bfs
  )
where

import           Control.Monad.Search

import           Test.Tasty
import           Test.Tasty.HUnit
import           Control.Egison hiding ( Integer )


splits :: [a] -> [([a], [a])]
splits xs = matchAllDFS xs (List Something)
              [[mc| $hs ++ $ts -> (hs, ts) |]]

get :: [a] -> [(a, [a])]
get xs = matchAllDFS xs (Set Something)
           [[mc| $x : $rs -> (x, rs) |]]


test_dfs :: [TestTree]
test_dfs =
  [
   testCase "splits"
    $ assertEqual "simple" [([], [1, 2]), ([1], [2]), ([1, 2], [])]
    $ toList $ dfs [1, 2] >>= fromList . splits >>= \(hs, ts) -> pure (hs, ts)
 , testCase "double cons"
    $ assertEqual "simple" [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
    $ toList $ dfs [1, 2, 3] >>= fromList . get >>= \(x, xs) -> (fromList . get) xs >>= \(y, _) -> pure (x, y)
   ]

test_bfs :: [TestTree]
test_bfs =
  [
   testCase "double cons"
    $ assertEqual "simple" [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(2,3),(3,2),(3,3)]
    $ toList $ bfs [1, 2, 3] >>= fromList . get >>= \(x, xs) -> (fromList . get) xs >>= \(y, _) -> pure (x, y)
 , testCase "infinite double cons"
    $ assertEqual "simple" [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2)]
    $ take 9 $ toList $ bfs [1..] >>= fromList . get >>= \(x, xs) -> (fromList . get) xs >>= \(y, _) -> pure (x, y)
 , testCase "guard"
    $ assertEqual "simple" [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7),(8,8),(9,9)]
    $ take 9 $ toList $ bfs [1..] >>= fromList . get >>= \(x, xs) -> (fromList . get) xs >>= \(y, _) -> guard (x == y) >> pure (x, y)
 , testCase "not pattern"
    $ assertEqual "simple" [(1,2),(2,1),(1,3),(3,1),(1,4),(2,3),(3,2),(4,1),(1,5)]
    $ take 9 $ toList $ bfs [1..] >>= fromList . get >>= \(x, xs) -> (fromList . get) xs >>= \(y, _) -> lnot (guard (x == y) >> pure ()) >> pure (x, y)
 , testCase "not pattern in do notation"
    $ assertEqual "simple" [(1,2),(2,1),(1,3),(3,1),(1,4),(2,3),(3,2),(4,1),(1,5)]
    $ take 9 $ toList $ do
       (x, xs) <- bfs [1..] >>= fromList . get
       (y, _) <- (fromList . get) xs
       return ()
       lnot (guard (x == y) >> pure ())
       pure (x, y)
   ]

