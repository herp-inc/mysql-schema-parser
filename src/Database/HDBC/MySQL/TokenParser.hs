module Database.HDBC.MySQL.TokenParser
    ( Token(..)
    , eof
    , identP
    , stringLiteralP
    , intLiteralP
    , eqP
    , semicolonP
    , commaP
    , paren
    , keywordP
    , keywordExpect
    , keywordExpectCI
    , tokenP
    , skipSpace
    , keywordsExpect
    , keywordsExpectCI
    , choice
    )
where

import qualified Data.Attoparsec.Text          as P
import           Data.Functor                   ( ($>) )
import           Data.Char                      ( isDigit
                                                , isAsciiUpper
                                                , isAsciiLower
                                                , toLower
                                                )
import           Control.Monad                  ( forM )
import           Control.Monad                  ( guard )

data Token = TKeyword String | TIdent String | TParen [Token] | TStringLiteral String | TIntLiteral Integer | TEq | TSemicolon | TComma deriving (Show, Eq)

choice :: [P.Parser a] -> P.Parser a
choice ps = skipSpace *> P.choice ps

tokenP :: P.Parser Token
tokenP = choice
    [ TKeyword <$> keywordP
    , TIdent <$> identP
    , TParen <$> paren (P.many' tokenP)
    , TStringLiteral <$> stringLiteralP
    , TIntLiteral <$> intLiteralP
    , TEq <$ eqP
    , TSemicolon <$ semicolonP
    , TComma <$ commaP
    ]

skipSpace :: P.Parser ()
skipSpace = P.skipSpace *> P.many' (commentP *> P.skipSpace) $> ()
  where
    blockCommentP = P.string "/*" *> P.manyTill P.anyChar (P.string "*/") $> ()
    lineCommentP  = P.string "--" *> P.manyTill P.anyChar P.endOfLine $> ()
    commentP      = P.choice [blockCommentP, lineCommentP]

eof :: P.Parser ()
eof = skipSpace *> P.endOfInput

-- 雑
identP :: P.Parser String
identP = skipSpace *> identP'
    where 
        identP' = bQuoted $ P.many' $ P.notChar '`'
        bQuoted p = P.char '`' *> p <* P.char '`'

-- 雑
stringLiteralP :: P.Parser String
stringLiteralP = skipSpace *> stringLiteralP'
    where 
        stringLiteralP' = sQuoted $ P.many' $ P.notChar '\''
        sQuoted p = P.char '\'' *> p <* P.char '\''

-- 123fみたいな表記可能だったら死ぬ。負の数未対応。0から始まってもOK
intLiteralP :: P.Parser Integer
intLiteralP = skipSpace *> intLiteralP'
    where intLiteralP' = read <$> P.many1' (P.satisfy isDigit)

eqP :: P.Parser ()
eqP = skipSpace *> eqP' where eqP' = P.char '=' $> ()

semicolonP :: P.Parser ()
semicolonP = skipSpace *> semicolonP' where semicolonP' = P.char ';' $> ()

commaP :: P.Parser ()
commaP = skipSpace *> commaP' where commaP' = P.char ',' $> ()

paren :: P.Parser a -> P.Parser a
paren p = skipSpace *> paren'
    where paren' = P.char '(' *> skipSpace *> p <* skipSpace <* P.char ')'

keywordP :: P.Parser String
keywordP = skipSpace *> keywordP'
  where
    keywordP' = (:) <$> P.satisfy isKeywordHeadChar <*> P.many'
        (P.satisfy isKeywordChar)
    isKeywordHeadChar c = isAsciiLower c || isAsciiUpper c || c == '_'
    isKeywordChar c = isKeywordHeadChar c || isDigit c

keywordExpect :: String -> P.Parser String
keywordExpect x = P.try $ do
    keyword <- keywordP
    guard $ keyword == x
    pure keyword

keywordsExpect :: [String] -> P.Parser [String]
keywordsExpect ts = P.try $ forM ts keywordExpect

keywordExpectCI :: String -> P.Parser String
keywordExpectCI x = P.try $ do
    keyword <- keywordP
    _ <- guard $ fmap toLower keyword == fmap toLower x
    pure keyword

keywordsExpectCI :: [String] -> P.Parser [String]
keywordsExpectCI ts = P.try $ forM ts keywordExpectCI
