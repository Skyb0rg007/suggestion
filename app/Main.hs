module Main where

import           Text.Megaparsec
import           Language.Suggestion.Types
import           Language.Suggestion.Parse
import           Language.Suggestion.Eval
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
    txt <- Text.IO.getContents
    case parse parseSexp "stdin" txt of
      Left err -> putStrLn (errorBundlePretty err)
      Right val -> do
          let env = mempty
          res <- runScheme env (eval val)
          case res of
            Left err -> print err
            Right x -> print x
