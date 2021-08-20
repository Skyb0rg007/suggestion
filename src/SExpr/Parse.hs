{-# LANGUAGE LambdaCase, TypeFamilies #-}
module SExpr.Parse
    ( SExprParser (..)
    , Reader
    , decodeOne
    ) where

import           Control.Monad         (void)
import           Data.Char             (isSpace)
import           Data.Functor          ((<&>))
import           Data.Functor.Identity (Identity (..))
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           SExpr.Types
import           Text.Megaparsec
-- import           Debug.Trace

type Reader m atom = m (SExpr atom) -> m (SExpr atom)

data SExprParser m atom = SExprParser
    { parseAtom :: m atom
    , readerMap :: Map Char (Reader m atom)
    , comment   :: Maybe (m ())
    }

eofFail :: MonadParsec e s m => m a
eofFail = do
    offset <- getOffset
    parseError $ FancyError offset $ Set.singleton (ErrorFail "Unexpected end of file")

parseGenericSExpr
    :: (MonadParsec e s m, Token s ~ Char)
    => m atom
    -> Map Char (Reader m atom)
    -> m ()
    -> m (SExpr atom)
parseGenericSExpr atom reader skip = do
    let sExpr = parseGenericSExpr atom reader skip <?> "s-expr"
    skip
    s <- lookAhead $ observing anySingle
    ret <- case s of
             Left _ -> eofFail
             Right '(' ->
                 anySingle >> skip >> parseList sExpr skip
             Right c
               | Just r <- Map.lookup c reader -> anySingle >> r sExpr
             Right c -> SAtom <$> atom
    skip
    pure ret

parseList
    :: (MonadParsec e s m, Token s ~ Char)
    => m (SExpr atom)
    -> m ()
    -> m (SExpr atom)
parseList sExpr skip = do
    i <- lookAhead $ observing anySingle
    case i of
      Left _    -> eofFail
      Right ')' -> single ')' >> pure SNil
      _         -> do
          car <- sExpr
          skip
          c <- lookAhead $ observing anySingle
          case c of
            Right '.' -> do
                anySingle
                cdr <- sExpr
                skip
                _ <- single ')'
                skip
                pure $ SCons car cdr
            Right ')' -> do
                anySingle
                skip
                pure $ SCons car SNil
            _ -> do
                cdr <- parseList sExpr skip
                pure $ SCons car cdr

buildSkip
    :: (MonadParsec e s m, Token s ~ Char)
    => Maybe (m ())
    -> m ()
buildSkip Nothing = void $ takeWhileP (Just "white space") isSpace

decodeOne'
    :: (MonadParsec e s m, Token s ~ Char)
    => SExprParser m atom
    -> m (SExpr atom)
decodeOne' spec = parseGenericSExpr
                    (parseAtom spec)
                    (readerMap spec)
                    (buildSkip (comment spec))

doParse
    :: (ShowErrorComponent e, Stream s, Monad m, VisualStream s, TraversableStream s)
    => ParsecT e s m a
    -> s
    -> m (Either String a)
doParse p t =
    runParserT p "" t <&> \case
      Left err -> Left (errorBundlePretty err)
      Right x  -> Right x

decodeOne
    :: (Ord e, ShowErrorComponent e, Monad m, Token s ~ Char, Stream s, VisualStream s, TraversableStream s)
    => SExprParser (ParsecT e s m) atom
    -> s
    -> m (Either String (SExpr atom))
decodeOne spec = doParse (parser <* eof)
    where
        parser = parseGenericSExpr
                    (parseAtom spec)
                    (readerMap spec)
                    (buildSkip (comment spec))

