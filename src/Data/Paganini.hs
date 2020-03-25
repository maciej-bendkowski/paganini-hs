{-|
Module      : Data.Paganini
Description : EDSL for the paganini multiparametric combinatorial specification tuner.
Copyright   : (c) Maciej Bendkowski 2020

License     : BSD3
Maintainer  : maciej.bendkowski@tcs.uj.edu.pl
Stability   : experimental

EDSL for the paganini multiparametric combinatorial specification tuner. The actual
tuning is performed by invoking the external paganini process. Its input specification
is constructed within a type-safe monadic action.
-}
module Data.Paganini
  (module Data.Paganini.Monad
  ,module Data.Paganini.Tuner
  ,initProgram) where

import Data.Paganini.Monad
import Data.Paganini.Tuner
import Data.Paganini.Expressions
