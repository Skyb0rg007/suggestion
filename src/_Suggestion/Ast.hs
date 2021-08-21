{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Suggestion.Ast
    ( module Suggestion.Ast
    ) where

import qualified Data.Text as Text
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)
import           Suggestion.Syntax
import           SExpr.Types

data Exp
    = Let [(Text, Exp)] Exp
    | Begin [Exp]
    | App (NonEmpty Exp)
    | Var Text
    | Quote (SExpr Atom)
    deriving (Show, Eq, Read)

fromSExp :: SExpr Atom -> Either String Exp
fromSExp = \case
    SList (SAtom (ASymbol "let") : SList binds : body) -> do
        let f (SList [SAtom (ASymbol x), e]) = (x,) <$> fromSExp e
            f _ = Left "Invalid let-bind"
        binds' <- traverse f binds
        body' <- traverse fromSExp body 
        pure $ Let binds' (Begin body')

    SList (SAtom (ASymbol "begin") : exps) -> Begin <$> traverse fromSExp exps

    SList [SAtom (ASymbol "quote"), form] -> Right (Quote form)
    form@(SAtom (AString _)) -> Right (Quote form)
    form@(SAtom (ABoolean _)) -> Right (Quote form)
    form@(SAtom (ANumber _)) -> Right (Quote form)
    form@(SAtom (AComment _)) -> Right (Quote form)

    SAtom (ASymbol var) ->
        if var `elem` ["let", "begin", "quote"]
           then Left $ "Invalid variable " ++ Text.unpack var
           else Right (Var var)

    SList (f : args) -> do
        f' <- fromSExp f
        args' <- traverse fromSExp args
        pure $ App (f' :| args')

    _ -> Left "Invalid form"
