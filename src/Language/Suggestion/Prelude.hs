{-# LANGUAGE OverloadedStrings #-}

module Language.Suggestion.Prelude where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Foldable             (traverse_)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Loc
import           Language.Suggestion.Eval
import           Language.Suggestion.Types

let_ :: Prim
let_ = Prim { primName = Just "let", primRun = run }
    where
        run :: [SchemeVal] -> Scheme SchemeVal
        run (L _ (List binds) : body) = do
            let unwrap (L _ (List [(L _ (Symbol x)), e])) = pure (x, e)
                unwrap bind = schemeError (locOf bind) "Invalid let bind" [bind]
            env <- schemeEnv
            unwrappedBinds <- traverse unwrap binds
            evaluatedBinds <- traverse (traverse eval) unwrappedBinds
            schemeWithEnv (HashMap.union (HashMap.fromList evaluatedBinds)) $
                case body of
                  [] -> pure (L noLoc Void)
                  _ -> do
                      traverse_ eval (init body)
                      eval (last body)
        run form = schemeError (locOf form) "Invalid let form" [L (locOf form) (List form)]

begin :: Prim
begin = Prim { primName = Just "begin", primRun = run }
    where
        run form = primRun let_ (L noLoc (List []) : form)


lambda :: Prim
lambda = Prim { primName = Just "lambda", primRun = run }
    where
        run :: [SchemeVal] -> Scheme SchemeVal
        run (L _ (List params) : body) = do
            let unwrap (L _ (Symbol x)) = pure x
                unwrap form = schemeError (locOf form) "Invalid lambda argument" [form]
            params' <- traverse unwrap params
            creationEnv <- schemeEnv
            let lambdaRun args =
                    if length args /= length params'
                       then schemeError (locOf args) "Wrong number of arguments given to lambda" [L (locOf args) (List args)]
                       else do
                           args' <- traverse eval args
                           schemeWithEnv (HashMap.union (HashMap.fromList (zip params' args'))) $
                               primRun begin body
            pure $ L noLoc $ Primitive $ Prim { primName = Nothing, primRun = lambdaRun }
        run form = schemeError (locOf form) "Invalid lambda form" [L (locOf form) (List form)]
            
print :: Prim
print = Prim { primName = Just "print", primRun = run }
    where
        run sexps = liftIO (putStrLn (show sexps)) >> pure (L noLoc Void)
