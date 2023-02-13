# paganini-hs

`paganini-hs` is an experimental EDSL (embedded domain specific language) meant
as a Haskell wrapper for
[paganini](https://github.com/maciej-bendkowski/paganini) -- a multiparametric
combinatorial specification tuner written in Python.

Example use:
```haskell
testBinTrees' = paganini $ do
  Let z <- variable' 100000
  Let b <- variable

  b .=. 1 + z * b^2
  tune b

  z' <- value z
  b' <- value b
  return [z',b']
```

For more examples, please visit our test suite.
