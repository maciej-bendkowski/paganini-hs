module Data.Paganini.Tuner
  (tune
  ,paganini
  ,debugPaganini
  ,PaganiniError(..)
  ) where

import           Prelude                    hiding (seq)

import           Control.Exception
import           Control.Monad.State.Strict

import qualified Data.Map.Strict            as Map

import           Data.Maybe                 (mapMaybe)

import           Data.Paganini.Expressions
import           Data.Paganini.Monad

import           System.Exit
import           System.IO
import           System.Process

import           Text.Read                  (readMaybe)

try' :: IO a -> IO (Either IOException a)
try' = Control.Exception.try

-- | Writes the given program into the given handle.
writePaganini :: Program -> Handle -> IO ()
writePaganini p = runPaganini' (problemStmt p)
  where runPaganini' s hout = hPutStr hout s

readValue :: String -> Maybe Double
readValue = readMaybe

-- | Errors occurring while interacting with paganini.
data PaganiniError
  = PythonProcessError Program    -- ^ Could not create a 'python3' process.
  | SolverFailure String Program  -- ^ Paganini could not solve the problem.
    deriving (Show)

errorProgram :: PaganiniError -> Program
errorProgram (PythonProcessError p) = p
errorProgram (SolverFailure _ p)    = p

instance Exception PaganiniError

-- | Tunes the given specification computation, running an external paganini process.
--   Its result is an IO action representing either an error or the actual computation value.
paganini :: Spec a -> IO (Either PaganiniError a)
paganini m = try $ evalStateT m initProgram

-- | Tunes the given specification computation, running an external paganini process.
--   Its result is the same as `paganini` however, in addition, it prints the underlying
--   paganini specification on the stderr.
debugPaganini :: Spec a -> IO (Either PaganiniError a)
debugPaganini m = do
  status <- try (runStateT m initProgram)
  case status of
    Right (x, p)  -> do
      hPutStrLn stderr (problemStmt p)
      return $ Right x
    Left err -> do
      let p = errorProgram err
      hPutStrLn stderr (problemStmt p)
      return $ Left err

-- | Given a target variable, tunes the underlying paganini specification.
--   Note: if the external paganini process fails, an IO error is thrown.
tune :: Variable -> Spec ()
tune v = do
  -- set target variable.
  modify (\s -> s { targetVar = Just v })

  p <- get
  status <- liftIO $ runPaganini p
  case status of
    Left err -> throw err
    Right p' -> put p'

runPaganini :: Program -> IO (Either PaganiniError Program)
runPaganini p = do

  pp <- try' $ createProcess
    (proc "python3" [])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe }

  case pp of
    Right (Just hin, Just hout, Just herr, ph) -> do

      writePaganini p hin
      exitCode <- waitForProcess ph

      case exitCode of
        ExitFailure _ -> do
          err <- hGetContents herr
          return $ Left (SolverFailure err p)

        _ -> do

          out <- hGetContents hout
          let xs = mapMaybe readValue (lines out)
          let ys = zip (variables p) xs
          return $ Right p { values = Map.fromList ys }

    _ -> return $ Left (PythonProcessError p)
