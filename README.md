# paganini-hs

`paganini-hs` is an experimental EDSL (embedded domain specific language) meant
as a Haskell wrapper for
[paganini](https://github.com/maciej-bendkowski/paganini) -- a multiparametric
combinatorial specification tuner written in Python. `paganini-hs` uses
[BinderAnn](https://github.com/OctopiChalmers/BinderAnn) to capture user-defined
variables and use them in the construction of `paganini` input specifications.

Example use:
```haskell
testBinTrees' = paganini @@ do
  Let z <- variable' 100000
  Let b <- variable

  b .=. 1 + z * b^2
  tune b

  z' <- value z
  b' <- value b
  return [z',b']
```

For more examples, please visit our test suite.
