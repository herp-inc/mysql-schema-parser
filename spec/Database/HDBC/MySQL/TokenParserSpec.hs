module Database.HDBC.MySQL.TokenParserSpec
    ( spec
    )
where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import qualified Data.Attoparsec.Text          as P
import           Database.HDBC.MySQL.TokenParser

spec :: Spec
spec = describe "Database.HDBC.MySQL.TokenParser" $ do
    describe "choice" $ do
        it "choice with space" $ do
            (P.parseOnly
                    (  (choice [keywordExpect "hoge", keywordExpect "foo"])
                    <* P.endOfInput
                    )
                    "  foo"
                )
                `shouldBe` (Right "foo")
    describe "tokenP" $ do
        it "keyword" $ do
            (P.parseOnly (tokenP <* P.endOfInput) "hoge")
                `shouldBe` (Right $ TKeyword "hoge")
        it "ident" $ do
            (P.parseOnly (tokenP <* P.endOfInput) "`hoge`")
                `shouldBe` (Right $ TIdent "hoge")
        it "paren" $ do
            (P.parseOnly (tokenP <* P.endOfInput) "(hoge`foo`)")
                `shouldBe` (Right $ TParen [TKeyword "hoge", TIdent "foo"])
        it "string literal" $ do
            (P.parseOnly (tokenP <* P.endOfInput) "'hoge'")
                `shouldBe` (Right $ TStringLiteral "hoge")
        it "int literal" $ do
            (P.parseOnly (tokenP <* P.endOfInput) "123")
                `shouldBe` (Right $ TIntLiteral 123)
        it "eq" $ do
            (P.parseOnly (tokenP <* P.endOfInput) "=") `shouldBe` (Right TEq)
        it "semicolon" $ do
            (P.parseOnly (tokenP <* P.endOfInput) ";")
                `shouldBe` (Right TSemicolon)
        it "comma" $ do
            (P.parseOnly (tokenP <* P.endOfInput) ",") `shouldBe` (Right TComma)
        it "with space" $ do
            (P.parseOnly (tokenP <* P.endOfInput) " \n  =")
                `shouldBe` (Right TEq)
    describe "skipSpace" $ do
        it "empty" $ do
            (P.parseOnly (skipSpace <* P.endOfInput) "") `shouldBe` (Right ())
        describe "space" $ do
            it "space" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) " ")
                    `shouldBe` (Right ())
            it "tab" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "\t")
                    `shouldBe` (Right ())
            it "\\n" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "\n")
                    `shouldBe` (Right ())
            it "\\r\\n" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "\r\n")
                    `shouldBe` (Right ())
            it "all" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "    \n\t  \r\n")
                    `shouldBe` (Right ())
        describe "line comment" $ do
            it "normal" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "--hoge\n")
                    `shouldBe` (Right ())
            it "empty" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "--\n")
                    `shouldBe` (Right ())
        describe "block comment" $ do
            it "normal" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "/*hoge*/")
                    `shouldBe` (Right ())
            it "empty" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "/**/")
                    `shouldBe` (Right ())
            it "nest ignore" $ do
                (P.parseOnly (skipSpace <* P.endOfInput) "/*/**/")
                    `shouldBe` (Right ())
        it "many" $ do
            (P.parseOnly (skipSpace <* P.endOfInput)
                         "  \n/*hoge*/\n--foo\r\n\t/*bar*/--\n  "
                )
                `shouldBe` (Right ())
            (P.parseOnly (skipSpace *> P.string "hoge" <* P.endOfInput)
                         "  \n/*hoge*/\n--foo\r\n\t/*bar*/--\n hoge"
                )
                `shouldBe` (Right "hoge")
    describe "eof" $ do
        it "empty" $ do
            (P.parseOnly eof "") `shouldBe` (Right ())
        it "space" $ do
            (P.parseOnly eof "/**/  ") `shouldBe` (Right ())
        it "not eof" $ do
            (P.parseOnly eof "hoge") `shouldBe` (Left "endOfInput")
    describe "identP" $ do
        it "empty" $ do
            (P.parseOnly (identP <* P.endOfInput) "``") `shouldBe` (Right "")
        it "normal" $ do
            (P.parseOnly (identP <* P.endOfInput) "`hoge`")
                `shouldBe` (Right "hoge")
        it "with space" $ do
            (P.parseOnly (identP <* P.endOfInput) " `hoge`")
                `shouldBe` (Right "hoge")
        it "no close quote" $ do
            (P.parseOnly identP "`hoge")
                `shouldBe` (Left "'`': not enough input")
    describe "stringLiteralP" $ do
        it "empty" $ do
            (P.parseOnly (stringLiteralP <* P.endOfInput) "''")
                `shouldBe` (Right "")
        it "normal" $ do
            (P.parseOnly (stringLiteralP <* P.endOfInput) "'hoge'")
                `shouldBe` (Right "hoge")
        it "with space" $ do
            (P.parseOnly (stringLiteralP <* P.endOfInput) " 'hoge'")
                `shouldBe` (Right "hoge")
        it "no close quote" $ do
            (P.parseOnly stringLiteralP "'hoge")
                `shouldBe` (Left "'\\'': not enough input")
    describe "intLiteralP" $ do
        it "normal" $ do
            (P.parseOnly (intLiteralP <* P.endOfInput) "123")
                `shouldBe` (Right 123)
        it "with space" $ do
            (P.parseOnly (intLiteralP <* P.endOfInput) " 123")
                `shouldBe` (Right 123)
        it "start 0" $ do
            (P.parseOnly (intLiteralP <* P.endOfInput) "0123")
                `shouldBe` (Right 123)
        it "minus" $ do
            (P.parseOnly intLiteralP "-123")
                `shouldBe` (Left "Failed reading: satisfy")
    describe "eqP" $ do
        it "normal" $ do
            (P.parseOnly (eqP <* P.endOfInput) "=") `shouldBe` (Right ())
        it "with space" $ do
            (P.parseOnly (eqP <* P.endOfInput) " =") `shouldBe` (Right ())
        it "multi" $ do
            (P.parseOnly (eqP <* P.char '=' <* P.endOfInput) "==")
                `shouldBe` (Right ())
        it "not eq" $ do
            (P.parseOnly eqP "-")
                `shouldBe` (Left "'=': Failed reading: satisfy")
    describe "semicolonP" $ do
        it "normal" $ do
            (P.parseOnly (semicolonP <* P.endOfInput) ";") `shouldBe` (Right ())
        it "with space" $ do
            (P.parseOnly (semicolonP <* P.endOfInput) " ;")
                `shouldBe` (Right ())
        it "multi" $ do
            (P.parseOnly (semicolonP <* P.char ';' <* P.endOfInput) ";;")
                `shouldBe` (Right ())
        it "not samicolon" $ do
            (P.parseOnly semicolonP "-")
                `shouldBe` (Left "';': Failed reading: satisfy")
    describe "commaP" $ do
        it "normal" $ do
            (P.parseOnly (commaP <* P.endOfInput) ",") `shouldBe` (Right ())
        it "with space" $ do
            (P.parseOnly (commaP <* P.endOfInput) " ,") `shouldBe` (Right ())
        it "multi" $ do
            (P.parseOnly (commaP <* P.char ',' <* P.endOfInput) ",,")
                `shouldBe` (Right ())
        it "not comma" $ do
            (P.parseOnly commaP "-")
                `shouldBe` (Left "',': Failed reading: satisfy")
    describe "paren" $ do
        it "normal" $ do
            (P.parseOnly (paren (P.char 'a') <* P.endOfInput) "(a)")
                `shouldBe` (Right 'a')
        it "with space" $ do
            (P.parseOnly (paren (P.char 'a') <* P.endOfInput) " (a)")
                `shouldBe` (Right 'a')
        it "with start space" $ do
            (P.parseOnly (paren (P.char 'a') <* P.endOfInput) "(  a)")
                `shouldBe` (Right 'a')
        it "with end space" $ do
            (P.parseOnly (paren (P.char 'a') <* P.endOfInput) "(a  )")
                `shouldBe` (Right 'a')
        it "with all space" $ do
            (P.parseOnly (paren (P.char 'a') <* P.endOfInput) "(   a  )")
                `shouldBe` (Right 'a')
        it "not close paren" $ do
            (P.parseOnly (paren $ P.char 'a') "(a")
                `shouldBe` (Left "')': not enough input")
    describe "keywordP" $ do
        it "normal" $ do
            (P.parseOnly (keywordP <* P.endOfInput) "hoge")
                `shouldBe` (Right "hoge")
        it "with number and symbol" $ do
            (P.parseOnly (keywordP <* P.endOfInput) "_hoge1__9__000")
                `shouldBe` (Right "_hoge1__9__000")
        it "start number" $ do
            (P.parseOnly keywordP "1hoge")
                `shouldBe` (Left "Failed reading: satisfy")
        it "with space" $ do
            (P.parseOnly (keywordP <* P.endOfInput) " hoge")
                `shouldBe` (Right "hoge")
    describe "keywordExpect" $ do
        it "normal" $ do
            (P.parseOnly (keywordExpect "hoge" <* P.endOfInput) "hoge")
                `shouldBe` (Right "hoge")
        it "with space" $ do
            (P.parseOnly (keywordExpect "hoge" <* P.endOfInput) " hoge")
                `shouldBe` (Right "hoge")
        it "not eq" $ do
            (P.parseOnly (keywordExpect "hoge") "hogea")
                `shouldBe` (Left "Failed reading: empty")
            (P.parseOnly (keywordExpect "hoge") "foo")
                `shouldBe` (Left "Failed reading: empty")
            (P.parseOnly (keywordExpect "hOgE") "Hoge")
                `shouldBe` (Left "Failed reading: empty")
        it "not consume" $ do
            (P.parseOnly
                    (  P.choice [keywordExpect "ab", keywordExpect "ac"]
                    <* P.endOfInput
                    )
                    "ac"
                )
                `shouldBe` (Right "ac")
    describe "keywordsExpect" $ do
        it "empty" $ do
            (P.parseOnly (keywordsExpect [] <* P.endOfInput) "")
                `shouldBe` (Right [])
        it "single" $ do
            (P.parseOnly (keywordsExpect ["hoge"] <* P.endOfInput) "hoge")
                `shouldBe` (Right ["hoge"])
        it "multi" $ do
            (P.parseOnly (keywordsExpect ["hoge", "foo"] <* P.endOfInput)
                         "hoge foo"
                )
                `shouldBe` (Right ["hoge", "foo"])
        it "with space" $ do
            (P.parseOnly (keywordsExpect ["hoge"] <* P.endOfInput) " hoge")
                `shouldBe` (Right ["hoge"])
        it "not eq" $ do
            (P.parseOnly (keywordsExpect ["hoge"]) "foo")
                `shouldBe` (Left "Failed reading: empty")
            (P.parseOnly (keywordsExpect ["hOgE"]) "Hoge")
                `shouldBe` (Left "Failed reading: empty")
            (P.parseOnly (keywordsExpect ["hoge", "foo"]) "hoge bar")
                `shouldBe` (Left "Failed reading: empty")
        it "not consume" $ do
            (P.parseOnly
                    (  P.choice
                            [ keywordsExpect ["hoge", "foo"]
                            , keywordsExpect ["hoge", "bar"]
                            ]
                    <* P.endOfInput
                    )
                    "hoge bar"
                )
                `shouldBe` (Right ["hoge", "bar"])
    describe "keywordExpectCI" $ do
        it "normal" $ do
            (P.parseOnly (keywordExpectCI "hoge" <* P.endOfInput) "hoge")
                `shouldBe` (Right "hoge")
        it "with space" $ do
            (P.parseOnly (keywordExpectCI "hoge" <* P.endOfInput) " hoge")
                `shouldBe` (Right "hoge")
        it "eq ci" $ do
            (P.parseOnly (keywordExpectCI "hOgE" <* P.endOfInput) "Hoge")
                `shouldBe` (Right "Hoge")
        it "not eq" $ do
            (P.parseOnly (keywordExpectCI "hoge") "hogea")
                `shouldBe` (Left "Failed reading: empty")
            (P.parseOnly (keywordExpectCI "hoge" <* P.endOfInput) "foo")
                `shouldBe` (Left "Failed reading: empty")
        it "not consume" $ do
            (P.parseOnly
                    (  P.choice [keywordExpectCI "ab", keywordExpectCI "ac"]
                    <* P.endOfInput
                    )
                    "ac"
                )
                `shouldBe` (Right "ac")
    describe "keywordsExpectCI" $ do
        it "empty" $ do
            (P.parseOnly (keywordsExpectCI [] <* P.endOfInput) "")
                `shouldBe` (Right [])
        it "single" $ do
            (P.parseOnly (keywordsExpectCI ["hoge"] <* P.endOfInput) "hoge")
                `shouldBe` (Right ["hoge"])
        it "multi" $ do
            (P.parseOnly (keywordsExpectCI ["HOGE", "foo"] <* P.endOfInput)
                         "hoge FOO"
                )
                `shouldBe` (Right ["hoge", "FOO"])
        it "with space" $ do
            (P.parseOnly (keywordsExpectCI ["hoge"] <* P.endOfInput) " hoge")
                `shouldBe` (Right ["hoge"])
        it "not eq" $ do
            (P.parseOnly (keywordsExpectCI ["hoge"]) "foo")
                `shouldBe` (Left "Failed reading: empty")
            (P.parseOnly (keywordsExpectCI ["hoge", "foo"]) "hoge bar")
                `shouldBe` (Left "Failed reading: empty")
        it "not consume" $ do
            (P.parseOnly
                    (  P.choice
                            [ keywordsExpectCI ["hoge", "foo"]
                            , keywordsExpectCI ["hoge", "bar"]
                            ]
                    <* P.endOfInput
                    )
                    "hoge bar"
                )
                `shouldBe` (Right ["hoge", "bar"])
