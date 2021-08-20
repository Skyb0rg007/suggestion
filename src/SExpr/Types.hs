{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module SExpr.Types
    ( SExpr (..)
    , pattern SList
    , pattern SDotted
    ) where

import           Control.Monad         (foldM)
import           Data.Bifunctor        (first)
import           Data.Data             (Data)
import           Data.Functor.Foldable
import           Data.String           (IsString (fromString))
import           Data.Typeable         (Typeable)
import           GHC.Exts              (IsList (Item, fromList, toList))

-- | Original representation of an S-Expression
data SExpr atom
    = SCons (SExpr atom) (SExpr atom)
    | SAtom atom
    | SNil
    deriving (Show, Eq, Read, Functor, Data, Typeable, Foldable, Traversable)

instance IsString atom => IsString (SExpr atom) where
    fromString = SAtom . fromString

instance IsList (SExpr atom) where
    type Item (SExpr atom) = SExpr atom
    fromList = foldr SCons SNil
    toList = go
        where go = \case
                SCons x xs -> x : go xs
                SNil -> []
                SAtom _ -> error "SExpr.toList - unable to turn atom into list"

pattern SList :: [SExpr atom] -> SExpr atom
pattern SList xs <- (toSList -> Just xs)
    where SList xs = fromSList xs

toSList :: SExpr atom -> Maybe [SExpr atom]
toSList = \case
    SAtom _    -> Nothing
    SNil       -> Just []
    SCons x xs -> (x :) <$> toSList xs

fromSList :: [SExpr atom] -> SExpr atom
fromSList = foldr SCons SNil

pattern SDotted :: [SExpr atom] -> atom -> SExpr atom
pattern SDotted xs x <- (toSDotted -> Just (xs, x))
    where SDotted xs x = fromSDotted xs x
{-# COMPLETE SAtom, SDotted, SList #-}

toSDotted :: SExpr atom -> Maybe ([SExpr atom], atom)
toSDotted SAtom{} = Nothing
toSDotted sexp = go sexp
    where go :: SExpr atom -> Maybe ([SExpr atom], atom)
          go = \case
            SAtom atom -> Just ([], atom)
            SNil       -> Nothing
            SCons x xs -> first (x :) <$> go xs

fromSDotted :: [SExpr atom] -> atom -> SExpr atom
fromSDotted xs x = foldr SCons (SAtom x) xs
