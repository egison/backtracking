# Backtracking Monad

This library provides a backtracking monad following Spivey's paper "Algebras for combinatorial search".

## Relationship with Egison Pattern Matching

We implement this library for embedding Egison pattern matching shallowlly.
For example, we are planning to translate the pattern-match expression
```
matchAllDFS [1, 2, 3] (List Something)
  [[mc| $hs ++ $x :: $ts -> (hs, ts) |]]
-- [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]
```
as follows:
```haskell
(\(mat, tgt) ->
   join mat tgt >>= \((m11, m12), (d11, d12)) ->
     let hs = d11 in
       cons m12 d12 >>= \((m21, m22), (d21, d22)) ->
         let x = d21 in
           let ts = d22 in
             pure (hs, x, ts))
  (dfs (List Something, [1, 2, 3]))
-- [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]
```

