# Backtracking Monad

This library provides a backtracking monad following Spivey's paper "Algebras for combinatorial search".

## Relationship with Egison Pattern Matching

We create this library for implementing [Sweet Egison](https://github.com/egison/sweet-egison), a shallow embedding Egison pattern matching.
For example, we translate the pattern-match expression
```
matchAllDFS [1, 2, 3] (List Something)
  [[mc| $hs ++ $x :: $ts -> (hs, ts) |]]
-- [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]
```
as follows:
```haskell
(\(mat, tgt) ->
   join mat tgt >>= \((m11, m12), (d11, d12)) ->
     let hs = d11 in id >>
       cons m12 d12 >>= \((m21, m22), (d21, d22)) ->
         let x = d21 in id >>
           let ts = d22 in id >>
             pure (hs, x, ts))
  (dfs (List Something, [1, 2, 3]))
-- [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]
```
A pattern-match expression with value patterns
```
matchAllDFS [1, 2, 3, 12] (Multiset Eql)
  [[mc| $x :: #(x + 10) :: _ -> x |]]
-- [2]
```
is translated as follows:
```haskell
(\(mat, tgt) ->
   cons mat tgt >>= \((m11, m12), (d11, d12)) ->
     let x = d11 in id >>
       cons m12 d12 >>= \((m21, m22), (d21, d22)) ->
         value m21 (x + 10) d21 >>
           id >>
             pure (hs, x, ts))
  (dfs (List Something, [1, 2, 3]))
-- [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]
```
