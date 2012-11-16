-- | A Test-Suite for Verse.

import Test.Framework

import Language.Verse.Parser.Test (tests)

main :: IO ()
main = defaultMain tests
