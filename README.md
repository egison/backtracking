# Backtracking Monad

This library provides a backtracking monad following Spivey's paper "Algebras for combinatorial search".

## Getting Started

The backtracking monad can be used in the similar way as the list monad.
We only need to specify a seach stragety (`dfs` or `bfs`) for the initial value and insert `fromList` and `toList` for conversion.
```
take 10 (toList (dfs [1..] >>= \ns -> fromList ns >>= \x -> fromList ns >>= \y -> pure (x, y)))
-- [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]

take 10 (toList (bfs [1..] >>= \ns -> fromList ns >>= \x -> fromList ns >>= \y -> pure (x, y)))
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]
```

## Relationship with Egison Pattern Matching

We create this library for implementing [Sweet Egison](https://github.com/egison/sweet-egison), a shallow embedding Egison pattern matching.
For example, the match clause `[mc| $x : #(x + 10) : _ -> (x, x + 10) |]` is transformed as follows:
```haskell
    \ (mat_a5sV, tgt_a5sW)
      -> let (tmpM_a5sX, tmpM_a5sY) = (consM mat_a5sV) tgt_a5sW
         in
           ((fromList (((cons (GP, GP)) mat_a5sV) tgt_a5sW))
              >>=
                (\ (tmpT_a5sZ, tmpT_a5t0)
                   -> let x = tmpT_a5sZ in
                      let (tmpM_a5t1, tmpM_a5t2) = (consM tmpM_a5sY) tmpT_a5t0
                      in
                        ((fromList (((cons (GP, WC)) tmpM_a5sY) tmpT_a5t0))
                           >>=
                             (\ (tmpT_a5t3, tmpT_a5t4)
                                -> ((fromList ((((value (x + 10)) ()) tmpM_a5t1) tmpT_a5t3))
                                      >>= (\ () -> pure (x, x + 10)))))))
```
