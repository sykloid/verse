-- | A parser for Verse syntax.
module Language.Verse.Parser (
    Parser,
    runParser
) where

import Control.Monad.Identity
import Text.Parsec (ParsecT, SourceName, ParseError, runParserT)

type Parser = ParsecT String () Identity

runParser :: Parser a -> SourceName -> String -> Either ParseError a
runParser p s i = runIdentity $ runParserT p () s i
