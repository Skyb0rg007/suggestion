{-# LANGUAGE OverloadedStrings #-}

module Suggestion.Syntax
    ( module Suggestion.Syntax
    ) where

import           Control.Monad (void)
import           Data.Functor.Identity (runIdentity)
import           SExpr.Types
import           SExpr.Parse
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Void (Void)

type Comment = [Either Text Annot]

data Annot
    = Ref Text
    deriving (Show, Eq, Ord, Read)

data Atom
    = AString Text
    | ABoolean Bool
    | ANumber Double
    | ASymbol Text
    | AComment Comment
    deriving (Show, Eq, Ord, Read)

type Parser = Parsec Void Text

atomReader :: SExprParser Parser Atom
atomReader = SExprParser
    { parseAtom = parseString <|> parseBool <|> parseNum <|> parseSym <|> parseComment
    , readerMap = mempty
    , comment   = Nothing
    }
    where
        parseString = label "string" $ fmap (AString . Text.pack) $
            char '"' *> L.charLiteral `manyTill` char '"'
        parseBool = label "boolean" $ choice
            [ ABoolean True <$ string "#true"
            , ABoolean True <$ string "#t"
            , ABoolean False <$ string "#false"
            , ABoolean False <$ string "#f"
            ]
        parseNum = label "number" $ fmap ANumber $
            L.signed (pure ()) L.float
        parseSym = label "symbol" $ fmap (ASymbol . Text.pack) $
            some (alphaNumChar <|> oneOf ("!$%&*+-./:<=>?@^_~" :: [Char]))
        parseComment = label "comment" $ fmap AComment $
            string "#|" *> commentSection `manyTill` string "|#"
        commentSection :: Parser (Either Text Annot)
        commentSection = choice
            [ Right <$> parseAnnot
            , fmap (Left . Text.pack) $
                anySingle `someTill` lookAhead (void (char '{') <|> void (string "|#"))
            ]
        parseAnnot = label "annotation" $ choice
            [ fmap (Ref . Text.pack) $
                char '{' *> (anySingle :: Parser Char) `manyTill` char '}'
            ]

sexpParse :: Text -> Either String (SExpr Atom)
sexpParse = runIdentity . decodeOne atomReader
