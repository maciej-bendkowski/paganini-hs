{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.Paganini.Monad (
  Spec,
  Let (..),
  Def (..),
  variable,
  variable',
  (.=.),
  value,
  Data.Paganini.Monad.seq,
  seq',
  ucyc,
  ucyc',
  cyc,
  cyc',
  mset,
  mset',
  set,
  set',
  Variable (),
  Constructor (),
  Expr (),
  Program (),
  Constraint (..),
  FromConstructor (..),
  FromVariable (..),
  Sampleable (..),
)
where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as Map

import Data.Paganini.Expressions

-- | Anonymous variable without parameter value.
anonym :: Variable
anonym = Variable{ident = Nothing, param = Nothing}

-- | Paganini monad.
type Spec = StateT Program IO

-- | Polymorphic variables.
newtype Let = Let (forall a. FromVariable a => a)

specVariable :: Variable -> Spec Let
specVariable v = return $ Let (fromVariable v)

nextVariable :: Spec Variable
nextVariable = do
  ctr <- gets counter
  modify (\s -> s{counter = ctr + 1})
  return $ anonym{ident = Just ('v' : show ctr)}

-- | Declares a variables with given parameter.
variable' :: Integer -> Spec Let
variable' n = do
  v <- nextVariable
  stmts <- gets statements
  let v' = v{param = Just n}
  modify (\s -> s{statements = stmts ++ [VarAssign v']})
  specVariable v'

-- | Declares a variable with no parameter.
variable :: Spec Let
variable = do
  v <- nextVariable
  stmts <- gets statements
  modify (\s -> s{statements = stmts ++ [VarAssign v]})
  specVariable v

-- | Introduces a variable definition.
(.=.) :: Variable -> Expr -> Spec ()
v .=. e = do
  stmts <- gets statements
  modify (\s -> s{statements = stmts ++ [VarDef v e]})

infix 5 .=.

{- | Extracts the variable tuning value.
   Note: if the specification is not tuned, then no tuning value is returned.
-}
value :: Variable -> Spec (Maybe Double)
value v = do
  xs <- gets values
  return $ v `Map.lookup` xs

-- | Polymorphic constructor variables.
newtype Def = Def (forall a. FromConstructor a => a)

constr :: String -> Expr -> Spec Def
constr s e = do
  v <- nextVariable
  let c = Constructor{var = v, func = s, arg = e, constraint = Nothing}

  stmts <- gets statements
  modify (\p -> p{statements = stmts ++ [ConstrDef c]})

  return (Def $ fromConstructor c)

constr' :: String -> Expr -> Constraint -> Spec Def
constr' s e ctr = do
  v <- nextVariable
  let c = Constructor{var = v, func = s, arg = e, constraint = Just ctr}

  stmts <- gets statements
  modify (\p -> p{statements = stmts ++ [ConstrDef c]})

  return (Def $ fromConstructor c)

-- | Class of types which can be sampled.
class Sampleable a where
  -- | Extracts the variable DDG in linear encoded form.
  --   Note: if the specification is not tuned, then no DDG is returned.
  ddg :: a -> Spec (Maybe [Int])

instance Sampleable Let where
  ddg (Let v) = do
    xs <- gets ddgs
    return $ v `Map.lookup` xs

instance Sampleable Def where
  ddg (Def v) = do
    xs <- gets ddgs
    return $ (var v) `Map.lookup` xs

-- | Sequence constructor.
seq :: Expr -> Spec Def
seq = constr "Seq"

-- | Constrained sequence constructor.
seq' :: Expr -> Constraint -> Spec Def
seq' = constr' "Seq"

-- | Unlabelled cycle constructor.
ucyc :: Expr -> Spec Def
ucyc = constr "UCyc"

-- | Constrained unlabelled cycle constructor.
ucyc' :: Expr -> Constraint -> Spec Def
ucyc' = constr' "UCyc"

-- | Multiset constructor.
mset :: Expr -> Spec Def
mset = constr "MSet"

-- | Constrained multiset constructor.
mset' :: Expr -> Constraint -> Spec Def
mset' = constr' "MSet"

-- | Set constructor.
set :: Expr -> Spec Def
set = constr "Set"

-- | Constrained set constructor.
set' :: Expr -> Constraint -> Spec Def
set' = constr' "Set"

-- | Labelled cycle constructor.
cyc :: Expr -> Spec Def
cyc = constr "Cyc"

-- | Constrained labelled cycle constructor.
cyc' :: Expr -> Spec Def
cyc' = constr "Cyc"
