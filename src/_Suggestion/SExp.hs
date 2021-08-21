
module Suggestion.SExp
    ( SExp (..)
    ) where

import           Data.Text (Text)
import           Data.Loc

data SExpNode
    = Ident Text
    | String Text
    | Comment Text

type SExp = L SExpNode

