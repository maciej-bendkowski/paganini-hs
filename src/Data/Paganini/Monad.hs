{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Data.Paganini.Monad
  (Spec
  ,Let(..)
  ,Def(..)
  ,variable
  ,variable'
  ,(.=.)
  ,value

  ,Data.Paganini.Monad.seq
  ,seq'
  ,ucyc
  ,ucyc'
  ,cyc
  ,cyc'
  ,mset
  ,mset'
  ,set
  ,set'

  ,Variable()
  ,Constructor()
  ,Expr()
  ,Program()
  ,Constraint(..)

  ,FromConstructor(..)
  ,FromVariable(..)
  ) where

import           BinderAnn.Monadic

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map

import           Data.Paganini.Expressions

-- | Anonymous variable without parameter value.
anonym :: Variable
anonym = Variable { ident = Nothing
                  , param = Nothing }

-- | Paganini monad.
type Spec = StateT Program IO

-- | Polymorphic variables.
newtype Let = Let (forall a . FromVariable a => a)

instance AnnotatedM Spec Let where
  annotateM m (Info name _) = do
    Let v <- m
    let v' = v { ident = name }

    stmts <- gets statements
    modify (\s -> s { statements = stmts ++ [VarAssign v'] })

    return (Let $ fromVariable v')

specVariable :: Variable -> Spec Let
specVariable v = return $ Let (fromVariable v)

-- | Declares a variables with given parameter.
variable' :: Integer -> Spec Let
variable' n = specVariable v
  where v = Variable { ident = Nothing
                     , param = Just n }

-- | Declares a variable with no parameter.
variable :: Spec Let
variable = specVariable v
  where v = Variable { ident = Nothing
                     , param = Nothing }

-- | Introduces a variable definition.
(.=.) :: Variable -> Expr -> Spec ()
v .=. e = do
  stmts <- gets statements
  modify (\s -> s { statements = stmts ++ [VarDef v e] })

infix 5 .=.

-- | Extracts the variable tuning value.
--   Note: if the specification is not tuned, then no tuning value is returned.
value :: Variable -> Spec (Maybe Double)
value v = do
  xs <- gets values
  return $ v `Map.lookup` xs

-- | Polymorphic constructor variables.
newtype Def = Def (forall a . FromConstructor a => a)

instance AnnotatedM Spec Def where
  annotateM m (Info name _) = do
    Def con <- m
    let v = (var con) { ident = name }
    let con' = con { var = v }

    stmts <- gets statements
    modify (\s -> s { statements = stmts ++ [ConstrDef con'] })

    return (Def $ fromConstructor con')

constr :: String -> Expr -> Spec Def
constr s e = return (Def $ fromConstructor c)
  where c = Constructor { var        = anonym
                        , func       = s
                        , arg        = e
                        , constraint = Nothing }

constr' :: String -> Expr -> Constraint -> Spec Def
constr' s e ctr = return (Def $ fromConstructor c)
  where c = Constructor { var        = anonym
                        , func       = s
                        , arg        = e
                        , constraint = Just ctr }

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
