{-# LANGUAGE OverloadedStrings #-}

module Language.Suggestion.Parse (parseSexp) where

import           Data.Maybe (catMaybes)
import           Data.Loc
import           Data.Char (chr)
import           Numeric (readHex)
import           Language.Suggestion.Types
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import           Text.Megaparsec hiding (Pos)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 line block
    where line = L.skipLineComment ";"
          block = empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Lexeme but also wrap in a location
locate :: Parser a -> Parser (L a)
locate p = do
    let getPos :: Parser Pos
        getPos = do
            offset <- getOffset
            SourcePos name line column <- getSourcePos
            pure $ Pos name (unPos line) (unPos column) offset
    p1 <- getPos
    res <- p
    p2 <- getPos
    sc
    pure (L (p1 <--> p2) res)

--

mnemonicEscape :: Parser Char
mnemonicEscape = choice
    [ '\a' <$ string "\\a"
    , '\b' <$ string "\\b"
    , '\t' <$ string "\\t"
    , '\n' <$ string "\\n"
    , '\r' <$ string "\\r"
    ]

inlineHexEscape :: Parser Char
inlineHexEscape = do
    string "\\x"
    n <- some hexDigitChar
    char ';'
    pure $ chr $ fst $ head $ readHex n

ident :: Parser Text
ident = fmap Text.pack $ choice
    [ (:) <$> initial <*> many subsequent
    , between (char '|') (char '|') $ many symbolElement
    ]
    where
        initial = letterChar <|> oneOf ("!$%&*/:<=>?^_~" :: [Char])
        subsequent = initial <|> digitChar <|> specialSubsequent
        specialSubsequent = oneOf ("+-.@" :: [Char])
        symbolElement = choice
            [ noneOf ("|\\" :: [Char])
            , inlineHexEscape
            , mnemonicEscape
            , '|' <$ string "\\|"
            ]

num :: Parser SchemeNum
num = Flonum <$> L.signed (pure ()) L.float

str :: Parser Text
str =
    fmap (Text.pack . catMaybes) $
        between (char '"') (char '"') $
            many (fmap Just element <|> Nothing <$ lineCont)
    where
        element = choice
            [ noneOf ("\\\"" :: [Char])
            , mnemonicEscape
            , '\"' <$ string "\\\""
            , '\\' <$ string "\\\\"
            , inlineHexEscape
            ]
        lineCont = do
            char '\\'
            hspace
            eol
            hspace

comment :: Parser SchemeComment
comment = string "#|" *> fmap collapse (element `manyTill` string "|#")
    where
        collapse = go ""
            where go acc [] = [Right acc]
                  go acc (Left x : xs) = Right acc : Left x : go "" xs
                  go acc (Right c : xs) = go (Text.snoc acc c) xs
        element = choice
            [ fmap (Left . Ref) $
                between (char '{') (char '}') $
                    ident
            , Right <$> anySingle
            ]

sexp :: Parser SchemeVal
sexp = choice
    [ label "number" $ locate (Number <$> num)
    , label "symbol" $ locate (Symbol <$> ident)
    , label "string" $ locate (String <$> str)
    , label "comment" $ locate (Comment <$> comment)
    , label "quote" quote
    , label "quasiquote" quasiquote
    , label "unquote" unquote
    , label "unquote-splicing" unquoteSplicing
    , try $ label "list" list
    , label "dotted list" dottedList
    ]

parseSexp :: Parser SchemeVal
parseSexp = sc *> sexp

list :: Parser SchemeVal 
list = locate $ do
    symbol "("
    xs <- many sexp
    char ')'
    pure $ List xs


dottedList :: Parser SchemeVal
dottedList = locate $ do
    symbol "("
    xs <- some sexp
    lexeme (char '.')
    x <- sexp
    char ')'
    pure $ DottedList xs x

quote, quasiquote, unquote, unquoteSplicing :: Parser SchemeVal
quote = locate $ do
    L loc _ <- locate (symbol "'")
    x <- sexp
    pure $ List [L loc (Symbol "quote"), x]
quasiquote = locate $ do
    L loc _ <- locate (symbol "`")
    x <- sexp
    pure $ List [L loc (Symbol "quote"), x]
unquote = locate $ do
    L loc _ <- locate (symbol ",")
    x <- sexp
    pure $ List [L loc (Symbol "unquote"), x]
unquoteSplicing = locate $ do
    L loc _ <- locate (symbol ",@")
    x <- sexp
    pure $ List [L loc (Symbol "unquote-splicing"), x]

