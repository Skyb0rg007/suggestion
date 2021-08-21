{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.Suggestion.Eval where

import           Data.Loc
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Language.Suggestion.Types
import           Control.Monad.Trans.Cont

eval :: SchemeVal -> Scheme SchemeVal
eval x = 
    case unLoc x of
      Symbol sym -> do
          env <- schemeEnv
          case HashMap.lookup sym env of
            Nothing -> schemeError (locOf x) "Undefined variable" [x]
            Just val -> pure $ reloc (locOf x) val
      List [] -> pure x
      List (f : args) ->
          eval f >>= \case
            L _ (Primitive (Prim _ run)) -> run args
            f' -> schemeError (locOf f) "Evaluation of non-function" [f']
      DottedList{} -> schemeError (locOf x) "Evaluation of improper list" [x]
      Number{} -> pure x
      String{} -> pure x
      Comment{} -> pure x
      Boolean{} -> pure x
      Primitive{} -> pure x
      ErrorObject{} -> pure x
    

