{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Language.Suggestion.Types where

import           Control.Exception
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.ByteString            (ByteString)
import           Data.HashMap.Strict        (HashMap)
import           Data.Loc
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import           Data.Vector                (Vector)

-- Environment

type Env = HashMap Text SchemeVal

data SchemeException = SchemeException SchemeVal
    deriving (Show, Eq, Exception)

newtype Scheme a = Scheme (ReaderT Env IO a)
    deriving newtype (Functor, Applicative, Monad, MonadIO)

runScheme :: Env -> Scheme a -> IO (Either SchemeVal a)
runScheme env (Scheme m) = 
    fmap Right (runReaderT m env)
    `catch` \(SchemeException e) -> pure (Left e)

schemeRaise :: SchemeVal -> Scheme a
schemeRaise = Scheme . throw . SchemeException

schemeError :: Loc -> Text -> [SchemeVal] -> Scheme a
schemeError loc msg irr = schemeRaise (L loc (ErrorObject msg irr))

schemeEnv :: Scheme Env
schemeEnv = Scheme ask

schemeWithEnv :: (Env -> Env) -> Scheme a -> Scheme a
schemeWithEnv f (Scheme m) = Scheme (local f m)

-- Values

type SchemeVal = L SchemeValNode

data Prim = Prim
    { primName :: Maybe Text
    , primRun :: [SchemeVal] -> Scheme SchemeVal
    }
instance Show Prim where
    show = show . fromMaybe "<prim>" . primName
instance Eq Prim where
    _ == _ = True
instance Ord Prim where
    compare _ _ = EQ

data SchemeValNode
    = Symbol Text
    | List [SchemeVal]
    | DottedList [SchemeVal] SchemeVal
    | Number SchemeNum
    | String Text
    | Comment SchemeComment
    | Boolean Bool
    | Primitive Prim
    | ErrorObject Text [SchemeVal]
    | Void
    -- | Vector (Vector SchemeVal)
    -- | ByteVector (ByteString)
    deriving (Show, Eq, Ord)

type SchemeComment = [Either Annot Text]
data Annot
    = Ref Text
    deriving (Show, Eq, Ord, Read)

data SchemeNum
    = Flonum Double
    -- | Fixnum Int32
    -- | Bignum Integer
    -- | Ratnum SchemeNum SchemeNum
    -- | Cpxnum SchemeNum SchemeNum
    deriving (Show, Eq, Ord, Read)

