
module Suggestion.Syntax
    ( module Suggestion.Syntax
    ) where

import           SExpr.Types
import           Data.Text (Text)

type Comment = [Either Text Annot]

data Annot
    = Ref Text

data Atom
    = AString Text
    | ABoolean Bool
    | ANumber Double
    | ASymbol Text
    | AComment Comment

