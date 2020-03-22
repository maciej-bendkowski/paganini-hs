module Data.Paganini.Utils where

import Prelude hiding (unlines)

parens :: ShowS -> ShowS
parens x = ("(" ++) . x . (")" ++)

combine :: String -> [ShowS] -> ShowS
combine _ [] = ("" ++)
combine _ [x] = x
combine s (x : xs)
  = x . (s ++) . combine s xs

unlines :: [ShowS] -> ShowS
unlines [] = ("" ++)
unlines [x] = x
unlines (x : xs) =
  x . ("\n" ++) . unlines xs

indent :: ShowS -> ShowS
indent x = ("\t" ++) . x

except :: [ShowS] -> ShowS
except xs = unlines
  [("try:" ++)
  ,unlines (map indent xs)
  ,("except Exception as e:" ++)
  ,indent ("print(e,file=sys.stderr)" ++)
  ,indent ("exit(1)" ++)]
