-- | Representation of a Verse document.
module Language.Verse.AST (
    Document(..),
    Block(..),
    Inline(..)
) where

-- | A 'Document' is a list of blocks.
data Document
    = Document [Block]
  deriving (Eq, Read, Show)

-- | A 'Block' is a top-level document element, typically separated by newlines.
data Block
    = Paragraph [Inline] -- ^ A block-level element with no markup over that of its constituent elements.
  deriving (Eq, Read, Show)

-- | An 'Inline' is a piece of text, usually separated by syntactic delimiters, many of which can
-- occur side-by-side inside a block-level element.
data Inline
    = Content String -- ^ A non-marked-up piece of text.
    | Transform String String -- ^ A transformed piece of text, with the transformation name.
  deriving (Eq, Read, Show)
