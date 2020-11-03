{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Data.Paganini.Expressions
  ( Variable(..)
  , Constructor(..)
  , Monomial(..)
  , Expr(..)
  , Constraint(..)
  , Program(..)
  , Stmt(..)
  , FromVariable(..)
  , FromConstructor(..)
  , ProgramType(..)
  , variables
  , problemStmt
  , initProgram
  , initProgram'
  )
where

import           Prelude                 hiding ( unlines )

import           Data.Maybe
import           Data.Paganini.Utils

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Data.MultiSet                  ( MultiSet )
import qualified Data.MultiSet                 as MultiSet

-- | Symbolic variables.
data Variable
  = Variable { ident :: Maybe String  -- ^ identifier
             , param :: Maybe Integer -- ^ expected parameter value
             }

instance Eq Variable where
  x == y = ident x == ident y

instance Ord Variable where
  x <= y = ident x <= ident y

instance Show Variable where
  showsPrec _ v = case ident v of
    Nothing -> ("[?]" ++)
    Just v' -> (v' ++)

-- | Symbolic monomials, i.e. a product of positive powers of variables.
data Monomial
 = Monomial { coeff :: Integer -- ^ multiplicative coefficient
            , power :: MultiSet Variable -- ^ powers of variables.
            }

-- | Checks if the given monomial represents a constant.
isConstant :: Monomial -> Bool
isConstant = MultiSet.null . power

instance Show Monomial where
  showsPrec _ m
    | isConstant m = shows (coeff m)
    | otherwise = case coeff m of
      -1 -> ("-" ++) . powers'
      0  -> ("" ++)
      1  -> powers'
      _  -> shows (coeff m) . ("*" ++) . powers'
   where
    ps      = MultiSet.toOccurList $ power m
    powers' = reduce $ map mono ps

    mono (v, 1) = shows v
    mono (v, k) = shows v . ("**" ++) . shows k

    reduce []       = ("" ++)
    reduce [x     ] = x
    reduce (x : xs) = x . (" * " ++) . reduce xs

-- | Lifts the given integer to a monomial.
constM :: Integer -> Monomial
constM n = Monomial { coeff = n, power = MultiSet.empty }

-- | Multiplies the given monomials.
mulM :: Monomial -> Monomial -> Monomial
mulM a b = Monomial { coeff = coeff a * coeff b
                    , power = power a `MultiSet.union` power b
                    }

-- | Negates the monomial's coefficient.
negateM :: Monomial -> Monomial
negateM a = a { coeff = negate (coeff a) }

-- | Symbolic expressions, i.e. multivariate polynomials.
newtype Expr
  = Expr { monomials   :: [Monomial] }

instance Show Expr where
  showsPrec _ e = ms
   where
    ms = reduce (monomials e)

    reduce []  = ("" ++)
    reduce [x] = shows x
    reduce (x : y : xs) | coeff y < 0 = shows x . reduce (y : xs)
                        | otherwise   = shows x . (" + " ++) . reduce (y : xs)

instance Num Expr where
  fromInteger n = Expr [constM n]

  Expr a + Expr b = Expr $ a ++ b

  a * b = Expr [ x `mulM` y | x <- monomials a, y <- monomials b ]

  negate a = Expr $ map negateM (monomials a)

  signum _ = 1
  abs = id

-- | Class of types derivable from variables.
class FromVariable a where
  fromVariable :: Variable -> a

instance FromVariable Variable where
  fromVariable = id

instance FromVariable Expr where
  fromVariable v = Expr [Monomial { coeff = 1, power = MultiSet.singleton v }]

-- | Constructor constraints.
newtype Constraint
  = Equal Integer -- ^ equality constraints

instance Show Constraint where
  showsPrec _ (Equal n) = ("= " ++) . shows n

-- | Symbolic constructor expressions.
data Constructor
 = Constructor { var        :: Variable         -- ^ variable referring to the constructor
               , func       :: String           -- ^ constructor name
               , arg        :: Expr             -- ^ expression argument
               , constraint :: Maybe Constraint -- ^ optional constraint
               }

instance Show Constructor where
  showsPrec _ c = shows (var c) . (" := " ++) . (func c ++) . parens x
   where
    x = shows (arg c) . y
    y = case constraint c of
      Nothing -> ("" ++)
      Just y' -> (", " ++) . shows y'

-- | Class of types derivable from constructors.
class FromConstructor a where
  fromConstructor :: Constructor -> a

instance FromConstructor Constructor where
  fromConstructor = id

instance FromConstructor Expr where
  fromConstructor = fromVariable . var

-- | Paganini statements.
data Stmt
  = VarAssign Variable     -- ^ variable assignment, such as v = Variable()
  | VarDef Variable Expr   -- ^ variable definition, such as spec.add(b, 1 + z * b**2)
  | ConstrDef Constructor  -- ^ constructor definition, such as v = Seq(e)

instance Show Stmt where
  showsPrec _ (VarAssign v) = ("def " ++) . shows v

  showsPrec _ (VarDef v e ) = shows v . (" := " ++) . shows e

  showsPrec _ (ConstrDef c) = shows c

-- | Converts the given statement into its paganini equivalent.
toPSpec :: Stmt -> ShowS
toPSpec (VarAssign v) = shows v . (" = pg.Variable(" ++) . p . (")" ++)
 where
  p = case param v of
    Nothing -> ("" ++)
    Just n  -> shows n

toPSpec (ConstrDef c) = shows (var c) . (" = pg." ++) . con
 where
  con = (func c ++) . parens x
  x   = shows (arg c) . y
  y   = case constraint c of
    Nothing         -> ("" ++)
    Just (Equal y') -> (", pg.eq" ++) . parens (shows y')

toPSpec (VarDef v e) =
  ("spec.add(" ++) . shows v . (", " ++) . shows e . (")" ++)

-- | Program type.
data ProgramType = Rational | Algebraic

instance Show ProgramType where
  showsPrec _ Rational  = ("pg.Type.RATIONAL" ++)
  showsPrec _ Algebraic = ("pg.Type.ALGEBRAIC" ++)

-- | Interactive paganini program.
data Program
 = Program { statements  :: [Stmt]              -- ^ consecutive paganini statements
           , values      :: Map Variable Double -- ^ variable values
           , targetVar   :: Maybe Variable      -- ^ target tuning variable
           , programType :: Maybe ProgramType   -- ^ program type
           , force       :: Bool                -- ^ whether to use Method.FORCE
           , counter     :: Int                 -- ^ variable counter
           }

-- | Lists, in order, all variables defined in the given program.
variables :: Program -> [Variable]
variables p = mapMaybe
  (\case
    VarAssign v -> Just v
    ConstrDef c -> Just $ var c
    _           -> Nothing
  )
  (statements p)

instance Show Program where
  showsPrec _ p =
    ("{" ++)
      . combine "; " (map shows $ statements p)
      . ("}" ++)
      . (" [" ++)
      . combine "; " (map printVar $ variables p)
      . ("] " ++)
      . printTargetVar
      . ("." ++)
   where
    printVar v = case v `Map.lookup` values p of
      Nothing -> shows v
      Just x  -> shows v . ("(" ++) . shows x . (")" ++)

    printTargetVar = case targetVar p of
      Nothing -> ("Unknown target variable" ++)
      Just v  -> ("Target variable: " ++) . shows v

-- | Initial paganini program.
initProgram :: Program
initProgram = Program { statements  = []
                      , values      = Map.empty
                      , targetVar   = Nothing
                      , programType = Nothing
                      , force       = False
                      , counter     = 0
                      }

initProgram' :: Program
initProgram' = initProgram { force = True }

-- | Converts the given program into a paganini representation.
problemStmt :: Program -> String
problemStmt p = unlines stmts' ""
 where
  stmts' =
    [ ("import sys" ++)
      , ("import paganini as pg" ++)
      , ("spec = pg.Specification()" ++)
      ]
      ++ spec
      ++ (params : runner : vs)

  spec   = map toPSpec (statements p)
  runner = except
    [ ("problem = spec.run_tuner(" ++) . targetV . paramsArg . method . (")" ++)
    , ("if problem == float(\"inf\"):" ++)
    , indent ("raise ValueError(\"Infeasible tuning problem.\")" ++)
    ]

  params = case programType p of
    Nothing -> ("" ++)
    Just t  -> ("params = pg.Params(" ++) . shows t . (")" ++)

  targetV = case targetVar p of
    Nothing -> ("" ++)
    Just v  -> shows v

  paramsArg = case programType p of
    Nothing -> ("" ++)
    Just _  -> (", params" ++)

  method = if force p
    then (", method = pg.Method.FORCE" ++)
    else (", method = pg.Method.STRICT" ++)

  vs = map (\v -> ("print (" ++) . shows v . (".value)" ++)) $ variables p
