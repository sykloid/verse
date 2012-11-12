-- | A parser for Verse syntax.
module Language.Verse.Parser (
    Parser,
    runParser,

    document,

    block,
    paragraph,

    inline,
    content,
) where

import Control.Monad.Identity

import Text.Parsec (ParsecT, SourceName, ParseError, runParserT)

import Language.Verse.AST

type Parser = ParsecT String () Identity

runParser :: Parser a -> SourceName -> String -> Either ParseError a
runParser p s i = runIdentity $ runParserT p () s i

document :: Parser Document
document = undefined

block :: Parser Block
block = undefined

paragraph :: Parser Block
paragraph = undefined

inline :: Parser Inline
inline = undefined

content :: Parser Inline
content = undefined
