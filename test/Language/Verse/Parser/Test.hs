-- | A Test-Suite for the Verse parser.

module Language.Verse.Parser.Test (tests) where

import Data.Function (on)

import Test.HUnit hiding (Test)

import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit

import Language.Verse.AST
import Language.Verse.Parser

import Text.Parsec.Error

instance Eq ParseError where
    (==) = (==) `on` errorMessages

case_document_singleBlock :: Assertion
case_document_singleBlock =
    runParser document "case_document_singleBlock" "xxx" @?= Right (Document [Paragraph [Content "xxx"]])

case_document_multiBlock :: Assertion
case_document_multiBlock =
    runParser document "case_document_multiBlock" "xxx\n\nyyy" @?=
        Right (Document [Paragraph [Content "xxx"], Paragraph [Content "yyy"]])

case_document_blankTerminated :: Assertion
case_document_blankTerminated =
    runParser document "case_document_blankTerminated" "xxx\n\n" @?=
        Right (Document [Paragraph [Content "xxx"]])

case_paragraph_basic :: Assertion
case_paragraph_basic =
    runParser paragraph "case_paragraph_basic" "xxx" @?= Right (Paragraph [Content "xxx"])

case_paragraph_linebreakSeparated :: Assertion
case_paragraph_linebreakSeparated =
    runParser paragraph "case_paragraph_linebreakSeparated" "xxx\nyyy" @?= Right (Paragraph [Content "xxx yyy"])

case_paragraph_linebreakTerminated :: Assertion
case_paragraph_linebreakTerminated =
    runParser paragraph "case_paragraph_linebreakTerminated" "xxx\nyyy\n" @?= Right (Paragraph [Content "xxx yyy"])

case_paragraph_blankTerminated :: Assertion
case_paragraph_blankTerminated =
    runParser paragraph "case_paragraph_blankTerminated" "xxx\n\nyyy" @?= Right (Paragraph [Content "xxx"])

case_content_basic :: Assertion
case_content_basic = runParser content "case_content_basic" "xxx" @?= Right (Content "xxx")

case_content_newlineTerminated :: Assertion
case_content_newlineTerminated =
    runParser content "case_content_newlineTerminated" "xxx\nyyy" @?= Right (Content "xxx")

case_content_reservedTerminated :: Assertion
case_content_reservedTerminated =
    runParser content "case_content_reservedTerminated" "xxx|" @?= Right (Content "xxx")

case_transform_basic :: Assertion
case_transform_basic =
    runParser transform "case_transform_basic" "{xxx|zzz}" @?= Right (Transform "xxx" "zzz")

case_transform_textNewlineSkipped :: Assertion
case_transform_textNewlineSkipped =
    runParser transform "case_transform_textNewlineSkipped" "{xxx\nyyy|zzz}" @?= Right (Transform "xxx yyy" "zzz")

tests :: [Test]
tests = [
        testGroup "Document" [
                testCase "Single-Block Document" case_document_singleBlock,
                testCase "Multi-Block Document" case_document_multiBlock,
                testCase "Blank-Terminated Document" case_document_blankTerminated
            ],
        testGroup "Block" [
                testGroup "Paragraph" [
                        testCase "Basic Paragraph" case_paragraph_basic,
                        testCase "Linebreak-Separated Paragraph" case_paragraph_linebreakSeparated,
                        testCase "Linebreak-Terminated Paragraph" case_paragraph_linebreakTerminated,
                        testCase "Blank-Terminated Paragraph" case_paragraph_blankTerminated
                    ]
            ],
        testGroup "Inline" [
                testGroup "Content" [
                        testCase "Basic Content" case_content_basic,
                        testCase "Newline Terminated Content" case_content_newlineTerminated,
                        testCase "Reserved-Character Terminated Content" case_content_reservedTerminated
                    ],
                testGroup "Transform" [
                        testCase "Basic Transform" case_transform_basic,
                        testCase "Text-Newline Skipped Transform" case_transform_textNewlineSkipped
                    ]
            ]
    ]
