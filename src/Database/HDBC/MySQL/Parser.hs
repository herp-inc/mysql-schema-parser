module Database.HDBC.MySQL.Parser where

import           Control.Applicative            ( optional )
import           Data.Maybe                     ( isJust
                                                , catMaybes
                                                , mapMaybe
                                                )
import           Database.HDBC.ColTypes         ( SqlColDesc(..)
                                                , SqlTypeId(..)
                                                )
import qualified Data.Attoparsec.Text          as P
import           Data.Functor                   ( ($>) )
import           Safe                           ( headDef )
import qualified Database.HDBC.MySQL.TokenParser
                                               as TP
import           Control.Monad                  ( guard )

-- $setup
-- >>> import Data.Attoparsec.Text (parseOnly, endOfInput)
-- >>> parse p = parseOnly (p <* endOfInput)

type Col = (String, SqlColDesc)
type Table = (String, [String], [Col])
type Schema = [Table]

makeSqlColDesc :: SqlTypeId -> Bool -> SqlColDesc
makeSqlColDesc t nullable = SqlColDesc { colType        = t
                                       , colSize        = Nothing
                                       , colOctetLength = Nothing
                                       , colDecDigits   = Nothing
                                       , colNullable    = Just nullable
                                       }

schemaP :: P.Parser Schema
schemaP = catMaybes <$> P.many' statP

statP :: P.Parser (Maybe Table)
statP = TP.choice
    [Just <$> createTableP, Nothing <$ dropTableP, Nothing <$ emptyStatP]

schemaParser :: P.Parser Schema
schemaParser = schemaP <* TP.eof

emptyStatP :: P.Parser ()
emptyStatP = TP.semicolonP

-- |
-- >>> parse dropTableP  "DROP TABLE IF EXISTS `users`;"
-- Right ()
--
dropTableP :: P.Parser ()
dropTableP =
    TP.keywordsExpectCI ["DROP", "TABLE", "IF", "EXISTS"]
        *> TP.identP
        *> TP.semicolonP
        $> ()

createTableP :: P.Parser Table
createTableP = do
    _          <- TP.keywordsExpectCI ["CREATE", "TABLE"]
    tableName  <- TP.identP
    (pk, cols) <- TP.paren colDefsP
    _          <- P.manyTill TP.tokenP (P.try TP.semicolonP)
    pure $ (tableName, pk, cols)

-- |
-- >>> parse columnTypeP "varchar(255)"
-- Right SqlVarCharT
columnTypeP :: P.Parser SqlTypeId
columnTypeP = TP.choice
    [ SqlBitT <$ TP.keywordExpectCI "BIT" <* optional lengthP
    , SqlTinyIntT
    <$ TP.keywordExpectCI "TINYINT"
    <* optional lengthP
    <* unsignedP
    <* zerofillP
    , SqlSmallIntT
    <$ TP.keywordExpectCI "SMALLINT"
    <* optional lengthP
    <* unsignedP
    <* zerofillP
    , SqlIntegerT -- 24bit
    <$ TP.keywordExpectCI "MEDIUMINT"
    <* optional lengthP
    <* unsignedP
    <* zerofillP
    , SqlIntegerT
    <$ TP.choice [TP.keywordExpectCI "INT",TP.keywordExpectCI "INTEGER"]
    <* optional lengthP
    <* unsignedP
    <* zerofillP
    , SqlBigIntT
    <$ TP.keywordExpectCI "BIGINT"
    <* optional lengthP
    <* unsignedP
    <* zerofillP
    , SqlDoubleT
    <$ TP.choice [TP.keywordExpectCI "REAL",TP.keywordExpectCI "DOUBLE"]
    <* optional lengthDecimalsP
    <* unsignedP
    <* zerofillP
    , SqlFloatT
    <$ TP.keywordExpectCI "FLOAT"
    <* optional lengthDecimalsP
    <* unsignedP
    <* zerofillP
    , SqlDecimalT
    <$ TP.keywordExpectCI "DECIMAL"
    <* optional lengthMaybeDecimalsP
    <* unsignedP
    <* zerofillP
    , SqlNumericT
    <$ TP.keywordExpectCI "NUMERIC"
    <* optional lengthMaybeDecimalsP
    <* unsignedP
    <* zerofillP
    , SqlDateT <$ TP.keywordExpectCI "DATE"
    , SqlTimeT <$ TP.keywordExpectCI "TIME" <* optional fspP
    , SqlUTCDateTimeT <$ TP.keywordExpectCI "TIMESTAMP" <* optional fspP
    , SqlTimestampT <$ TP.keywordExpectCI "DATETIME" <* optional fspP
    , SqlIntegerT <$ TP.keywordExpectCI "YEAR"
    , SqlCharT
    <$ TP.keywordExpectCI "CHAR"
    <* optional lengthP
    <* optional characterSetP
    <* optional collateP
    , SqlVarCharT
    <$ TP.keywordExpectCI "VARCHAR"
    <* lengthP
    <* optional characterSetP
    <* optional collateP
    , SqlBinaryT <$ TP.keywordExpectCI "BINARY" <* optional lengthP
    , SqlVarBinaryT <$ TP.keywordExpectCI "VARBINARY" <* lengthP
    , SqlLongVarBinaryT <$ TP.keywordExpectCI "TINYBLOB"
    , SqlLongVarBinaryT <$ TP.keywordExpectCI "BLOB"
    , SqlLongVarBinaryT <$ TP.keywordExpectCI "MEDIUMBLOB"
    , SqlLongVarBinaryT <$ TP.keywordExpectCI "LONGBLOB"
    , SqlWLongVarCharT
    <$ TP.keywordExpectCI "TINYTEXT"
    <* binaryP
    <* optional characterSetP
    <* optional collateP
    , SqlWLongVarCharT
    <$ TP.keywordExpectCI "TEXT"
    <* binaryP
    <* optional characterSetP
    <* optional collateP
    , SqlWLongVarCharT
    <$ TP.keywordExpectCI "MEDIUMTEXT"
    <* binaryP
    <* optional characterSetP
    <* optional collateP
    , SqlWLongVarCharT
    <$ TP.keywordExpectCI "LONGTEXT"
    <* binaryP
    <* optional characterSetP
    <* optional collateP
    , SqlUnknownT "ENUM"
    <$ TP.keywordExpectCI "ENUM"
    <* (P.many' $ P.try TP.tokenP)
    <* optional characterSetP
    <* optional collateP
    , SqlUnknownT "SET"
    <$ TP.keywordExpectCI "SET"
    <* (P.many' $ P.try TP.tokenP)
    <* optional characterSetP
    <* optional collateP
    , SqlUnknownT "GEOMETRY" <$ TP.keywordExpectCI "GEOMETRY"
    , SqlUnknownT "POINT" <$ TP.keywordExpectCI "POINT"
    , SqlUnknownT "LINESTRING" <$ TP.keywordExpectCI "LINESTRING"
    , SqlUnknownT "POLYGON" <$ TP.keywordExpectCI "POLYGON"
    , SqlUnknownT "MULTIPOINT" <$ TP.keywordExpectCI "MULTIPOINT"
    , SqlUnknownT "MULTILINESTRING" <$ TP.keywordExpectCI "MULTILINESTRING"
    , SqlUnknownT "MULTIPOLYGON" <$ TP.keywordExpectCI "MULTIPOLYGON"
    , SqlUnknownT "GEOMETRYCOLLECTION" <$ TP.keywordExpectCI "GEOMETRYCOLLECTION"
    ]
  where
    lengthP         = TP.paren TP.intLiteralP
    lengthDecimalsP = TP.paren (TP.intLiteralP *> TP.commaP *> TP.intLiteralP)
    lengthMaybeDecimalsP =
        TP.paren (TP.intLiteralP *> optional (TP.commaP *> TP.intLiteralP))
    fspP          = TP.paren TP.intLiteralP
    unsignedP     = optional $ TP.keywordExpectCI "UNSIGNED"
    zerofillP     = optional $ TP.keywordExpectCI "ZEROFILL"
    characterSetP = TP.keywordsExpectCI ["CHARACTER", "SET"] *> TP.keywordP
    collateP      = TP.keywordExpectCI "COLLATE" *> TP.keywordP
    binaryP       = optional $ TP.keywordExpectCI "BINARY"


-- http://hackage.haskell.org/package/relational-query-HDBC-0.7.2.0/docs/src/Database.HDBC.Schema.MySQL.html#driverMySQL

-- |
-- >>> parse columnP "  `id` varchar(26) NOT NULL"
-- Right ("id",SqlColDesc {colType = SqlVarCharT, colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False})
--
-- >>> parse columnP "  `created_at` datetime NOT NULL"
-- Right ("created_at",SqlColDesc {colType = SqlTimestampT, colSize = Nothing, colOctetLength = Nothing, colDecDigits = Nothing, colNullable = Just False})
columnP :: P.Parser Col
columnP = do
    name    <- TP.identP
    colType <- columnTypeP
    notNull <- isJust <$> optional (TP.keywordsExpectCI ["NOT", "NULL"])
    _       <- skipNotCommaTokens
    pure (name, makeSqlColDesc colType $ not notNull)

pkP :: P.Parser [String]
pkP = TP.keywordsExpectCI ["PRIMARY", "KEY"]
    *> TP.paren (P.sepBy TP.identP (P.try TP.commaP))

otherColDefP :: P.Parser ()
otherColDefP = skipNotCommaTokens

skipNotCommaTokens :: P.Parser ()
skipNotCommaTokens = P.many' notCommaTokenP $> ()
  where
    notCommaTokenP = do
        t <- P.try TP.tokenP
        guard $ t /= TP.TComma


data ColDef = CdCol Col | CdPK [String] deriving (Show, Eq)

colDefP :: P.Parser (Maybe ColDef)
colDefP = TP.choice
    [(Just . CdCol) <$> columnP, (Just . CdPK) <$> pkP, Nothing <$ otherColDefP]


colDefsP :: P.Parser ([String], [Col])
colDefsP = do
    colDefs <- catMaybes <$> P.sepBy colDefP (P.try TP.commaP)
    pure (pickPk colDefs, filterCols colDefs)
  where
    pickPk = headDef [] . mapMaybe
        (\case
            CdPK  x -> Just x
            CdCol _ -> Nothing
        )

    filterCols = mapMaybe
        (\case
            CdCol x -> Just x
            CdPK  _ -> Nothing
        )

