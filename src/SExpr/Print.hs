
-- This code is copied from s-cargot
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SExpr.Print
    ( encode
    , encodeOne
    , Printer (..)
    , Indent (..)
    ) where

import           Data.Foldable
import           Data.Sequence          (Seq)
import qualified Data.Sequence          as Seq
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy.Text
import qualified Data.Text.Lazy         as Lazy (Text)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import           SExpr.Types

data Indent
    = Swing
    | SwingAfter Int
    | Align
    deriving (Show, Eq)

data Printer atom = Printer
    { atomPrinter  :: atom -> Text
    , swingIndent  :: SExpr atom -> Indent
    , indentAmount :: Int
    , maxWidth     :: Maybe Int
    , indentPrint  :: Bool
    }

data Size = Size
    { sizeSum :: !Int
    , sizeMax :: !Int
    }
    deriving (Show)

data Intermediate
    = IAtom Text
    | IList Indent Size Intermediate (Seq Intermediate) (Maybe Text)
    | IEmpty
    deriving (Show)

sizeOf :: Intermediate -> Size
sizeOf IEmpty = Size 2 2
sizeOf (IList _ (Size n m) _ _ _) = Size (n + 2) (m + 2)
sizeOf (IAtom t) =
    let len = Text.length t
     in Size len len

concatSize :: Size -> Size -> Size
concatSize (Size s1 m1) (Size s2 m2) = Size (s1 + 1 + s2) (max m1 m2)

toIntermediate :: forall atom. Printer atom -> SExpr atom -> Intermediate
toIntermediate printer = headOf
    where
        headOf :: SExpr atom -> Intermediate
        headOf (SAtom a) = IAtom (atomPrinter printer a)
        headOf SNil = IEmpty
        headOf (SCons x xs) =
            gather (swingIndent printer x) (headOf x) Seq.empty (sizeOf (headOf x)) xs

        gather :: Indent -> Intermediate -> Seq Intermediate -> Size -> SExpr atom -> Intermediate
        gather sw hd rs sz = \case
            SNil -> IList sw sz hd rs Nothing
            SAtom a -> IList sw (sz `concatSize` aSize) hd rs (Just aStr)
                where aSize = Size (Text.length aStr) (Text.length aStr)
                      aStr = atomPrinter printer a
            SCons x xs ->
                gather sw hd (rs Seq.|> headOf x) (sz `concatSize` sizeOf (headOf x)) xs

unboundIndentPrintSExpr :: Printer a -> SExpr a -> Lazy.Text
unboundIndentPrintSExpr spec = finalize . go . toIntermediate spec
  where
    finalize = Builder.toLazyText . joinLinesS

    go :: Intermediate -> Seq.Seq Builder
    go (IAtom t) = Seq.singleton (Builder.fromText t)
    go IEmpty    = Seq.singleton (Builder.fromString "()")
    -- this case should never be called with an empty argument to
    -- @values@, as that should have been translated to @IEmpty@
    -- instead.
    go (IList iv _ initial values rest)
      -- if we're looking at an s-expression that has no nested
      -- s-expressions, then we might as well consider it flat and let
      -- it take the whole line
      | Just strings <- traverse ppBasic (initial Seq.<| values) =
        Seq.singleton (Builder.singleton '(' <> buildUnwords strings <> pTail rest)

      -- it's not "flat", so we might want to swing after the first thing
      | Swing <- iv =
          -- if this match fails, then it means we've failed to
          -- convert to an Intermediate correctly!
          let butLast = insertParen (go initial) <> fmap doIndent (foldMap go values)
          in handleTail rest butLast

      -- ...or after several things
      | SwingAfter n <- iv =
          let (hs, xs) = Seq.splitAt n (initial Seq.<| values)
              hd = Builder.singleton '(' <> buildUnwords (foldMap go hs)
              butLast = hd Seq.<| fmap doIndent (foldMap go xs)
          in handleTail rest butLast

      -- the 'align' choice is clunkier because we need to know how
      -- deep to indent, so we have to force the first builder to grab its size
      | otherwise =
        let -- so we grab that and figure out its length plus two (for
            -- the leading paren and the following space). This uses a
            -- max because it's possible the first thing is itself a
            -- multi-line s-expression (in which case it seems like
            -- using the Align strategy is a terrible idea, but who am
            -- I to quarrel with the wild fruits upon the Tree of
            -- Life?)
            len = 2 + maximum (fmap (Lazy.Text.length . Builder.toLazyText) (go initial))
        in case Seq.viewl values of
          -- if there's nothing after the head of the expression, then
          -- we simply close it
          Seq.EmptyL -> insertParen (insertCloseParen (go initial))
          -- otherwise, we put the first two things on the same line
          -- with spaces and everything else gets indended the
          -- forementioned length
          y Seq.:< ys ->
            let hd = Builder.singleton '(' <> buildUnwords (foldMap go (Seq.fromList [initial, y]))
                butLast = hd Seq.<| fmap (doIndentOf (fromIntegral len)) (foldMap go ys)
            in handleTail rest butLast

    doIndent :: Builder -> Builder
    doIndent = doIndentOf (indentAmount spec)

    doIndentOf :: Int -> Builder -> Builder
    doIndentOf n b = Builder.fromText (Text.replicate n " ") <> b

    insertParen :: Seq.Seq Builder -> Seq.Seq Builder
    insertParen s = case Seq.viewl s of
      Seq.EmptyL -> s
      x Seq.:< xs -> (Builder.singleton '(' <> x) Seq.<| xs

    handleTail :: Maybe Text -> Seq.Seq Builder -> Seq.Seq Builder
    handleTail Nothing = insertCloseParen
    handleTail (Just t) =
      (Seq.|> (Builder.fromString " . " <> Builder.fromText t <> Builder.singleton ')'))

    insertCloseParen :: Seq.Seq Builder -> Seq.Seq Builder
    insertCloseParen s = case Seq.viewr s of
      Seq.EmptyR -> Seq.singleton (Builder.singleton ')')
      xs Seq.:> x -> xs Seq.|> (x <> Builder.singleton ')')

    buildUnwords sq =
      case Seq.viewl sq of
      Seq.EmptyL -> mempty
      t Seq.:< ts -> t <> foldMap (\ x -> Builder.singleton ' ' <> x) ts

    pTail Nothing = Builder.singleton ')'
    pTail (Just t) = Builder.fromString " . " <> Builder.fromText t <> Builder.singleton ')'

    ppBasic (IAtom t) = Just (Builder.fromText t)
    ppBasic (IEmpty) = Just (Builder.fromString "()")
    ppBasic _ = Nothing

spaceDot :: Builder
spaceDot = Builder.singleton ' ' <> Builder.singleton '.' <> Builder.singleton ' '

indent :: Int -> Builder -> Builder
indent n ts = mconcat [ Builder.singleton ' ' | _ <- [1..n] ] <> ts

joinLinesS :: Seq.Seq Builder -> Builder
joinLinesS Seq.Empty = ""
joinLinesS (t Seq.:<| ts)
  | null ts = t
  | otherwise = t <> Builder.fromString "\n" <> joinLinesS ts

unwordsS :: Seq Builder -> Builder
unwordsS Seq.Empty = ""
unwordsS (t Seq.:<| ts)
  | null ts = t
  | otherwise = t <> " " <> unwordsS ts

indentAllS :: Int -> Seq Builder -> Builder
indentAllS n = ("\n" <>) . joinLinesS . fmap (indent n)

indentSubsequentS :: Int -> Seq Builder -> Builder
indentSubsequentS n Seq.Empty = ""
indentSubsequentS n (t Seq.:<| ts)
  | null ts = t
  | otherwise = joinLinesS (t Seq.:<| fmap (indent n) ts)

prettyPrintSExpr :: Printer a -> SExpr a -> Lazy.Text
prettyPrintSExpr pr expr =
    case maxWidth pr of
      Nothing
        | indentPrint pr -> unboundIndentPrintSExpr pr expr
        | otherwise -> flatPrintSExpr (atomPrinter pr <$> expr)
      Just w -> indentPrintSExpr' w pr expr

indentPrintSExpr' :: Int -> Printer a -> SExpr a -> Lazy.Text
indentPrintSExpr' maxAmt pr = Builder.toLazyText . pp 0 . toIntermediate pr
    where
        pp ind = \case
            IEmpty -> Builder.fromString "()"
            IAtom t -> Builder.fromText t
            IList i sz h values end ->
                Builder.singleton '(' <> hd <> body <> tl <> Builder.singleton ')'
                where
                    tl = case end of
                           Nothing -> mempty
                           Just x -> Builder.fromString " . " <> Builder.fromText x
                    hd = pp (ind + 1) h
                    headWidth = sizeSum (sizeOf h)
                    indented =
                        case i of
                          SwingAfter n ->
                              let (l, ls) = Seq.splitAt n values
                                  t = unwordsS (pp (ind + 1) <$> l)
                                  ts = indentAllS (ind + indentAmount pr) (pp (ind + indentAmount pr) <$> ls)
                               in t <> ts
                          Swing ->
                              indentAllS (ind + indentAmount pr) (pp (ind + indentAmount pr) <$> values)
                          Align ->
                              indentSubsequentS (ind + headWidth + 1) (pp (ind + headWidth + 1) <$> values)
                    body
                      | length values == 0 = mempty
                      | sizeSum sz + ind > maxAmt = Builder.singleton ' ' <> indented
                      | otherwise = Builder.singleton ' ' <> unwordsS (pp (ind + 1) <$> values)

flatPrintSExpr :: SExpr Text -> Lazy.Text
flatPrintSExpr = Builder.toLazyText . pHead
    where
        pHead = \case
            SCons x xs -> Builder.singleton '(' <> pHead x <> pTail xs
            SAtom t -> Builder.fromText t
            SNil -> Builder.singleton '(' <> Builder.singleton ')'

        pTail = \case
            SCons x xs -> Builder.singleton ' ' <> pHead x <> pTail xs
            SAtom t -> spaceDot <> Builder.fromText t <> Builder.singleton ')'
            SNil -> Builder.singleton ')'

encodeOne :: Printer atom -> SExpr atom -> Text
encodeOne pr = Lazy.Text.toStrict . prettyPrintSExpr pr

encode :: Printer atom -> [SExpr atom] -> Text
encode pr = Text.intercalate "\n\n" . map (encodeOne pr)
