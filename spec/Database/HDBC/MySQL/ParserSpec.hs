module Database.HDBC.MySQL.ParserSpec
    ( spec
    )
where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import           Database.HDBC.MySQL.Parser
import qualified Data.Attoparsec.Text          as P
import           Database.HDBC.ColTypes         ( SqlColDesc(..)
                                                , SqlTypeId(..)
                                                )

spec :: Spec
spec = describe "Database.HDBC.MySQL.Parser" $ do
    describe "makeSqlColDesc" $ do
        it "makeSqlColDesc" $ do
            (makeSqlColDesc SqlIntegerT True) `shouldBe` SqlColDesc
                { colType        = SqlIntegerT
                , colSize        = Nothing
                , colOctetLength = Nothing
                , colDecDigits   = Nothing
                , colNullable    = Just True
                }
            (makeSqlColDesc SqlVarCharT False) `shouldBe` SqlColDesc
                { colType        = SqlVarCharT
                , colSize        = Nothing
                , colOctetLength = Nothing
                , colDecDigits   = Nothing
                , colNullable    = Just False
                }
    describe "schemaP" $ do
        it "create table" $ do
            (P.parseOnly
                    (schemaP <* P.endOfInput)
                    (  "CREATE TABLE `hoge` (\n"
                    <> "  `id` varchar(36) NOT NULL,\n"
                    <> "  `created_at` datetime(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),\n"
                    <> "  PRIMARY KEY (`id`),\n"
                    <> "  KEY `IDX` (`created_at`)\n"
                    <> ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"
                    )
                )
                `shouldBe` (Right
                               [ ( "hoge"
                                 , ["id"]
                                 , [ ("id", makeSqlColDesc SqlVarCharT False)
                                   , ( "created_at"
                                     , makeSqlColDesc SqlTimestampT False
                                     )
                                   ]
                                 )
                               ]
                           )
    describe "statP" $ do
        it "create table" $ do
            (P.parseOnly
                    (statP <* P.endOfInput)
                    (  "CREATE TABLE `hoge` (\n"
                    <> "  `id` varchar(36) NOT NULL,\n"
                    <> "  `created_at` datetime(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),\n"
                    <> "  PRIMARY KEY (`id`),\n"
                    <> "  KEY `IDX` (`created_at`)\n"
                    <> ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"
                    )
                )
                `shouldBe` (Right $ Just
                               ( "hoge"
                               , ["id"]
                               , [ ("id", makeSqlColDesc SqlVarCharT False)
                                 , ( "created_at"
                                   , makeSqlColDesc SqlTimestampT False
                                   )
                                 ]
                               )
                           )
    describe "schemaParser" $ do
        it "create table" $ do
            (P.parseOnly
                    schemaParser
                    (  "CREATE TABLE `hoge` (\n"
                    <> "  `id` varchar(36) NOT NULL,\n"
                    <> "  `created_at` datetime(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),\n"
                    <> "  PRIMARY KEY (`id`),\n"
                    <> "  KEY `IDX_7e73af02f23a3729b239d7ec8f` (`created_at`)\n"
                    <> ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"
                    )
                )
                `shouldBe` (Right
                               [ ( "hoge"
                                 , ["id"]
                                 , [ ("id", makeSqlColDesc SqlVarCharT False)
                                   , ( "created_at"
                                     , makeSqlColDesc SqlTimestampT False
                                     )
                                   ]
                                 )
                               ]
                           )
    describe "emptyStatP" $ do
        it "empty stat" $ do
            (P.parseOnly (emptyStatP <* P.endOfInput) "  ;")
                `shouldBe` (Right ())
    describe "dropTableP" $ do
        it "drop table" $ do
            (P.parseOnly (dropTableP <* P.endOfInput)
                         "DROP TABLE IF EXISTS `hoge`;"
                )
                `shouldBe` (Right ())
    describe "createTableP" $ do
        it "create table" $ do
            (P.parseOnly
                    (createTableP <* P.endOfInput)
                    (  "CREATE TABLE `hoge` (\n"
                    <> "  `id` varchar(36) NOT NULL,\n"
                    <> "  `created_at` datetime(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),\n"
                    <> "  PRIMARY KEY (`id`),\n"
                    <> "  KEY `IDX` (`created_at`)\n"
                    <> ") ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;"
                    )
                )
                `shouldBe` (Right
                               ( "hoge"
                               , ["id"]
                               , [ ("id", makeSqlColDesc SqlVarCharT False)
                                 , ( "created_at"
                                   , makeSqlColDesc SqlTimestampT False
                                   )
                                 ]
                               )
                           )
    describe "columnTypeP" $ do
        it "bit" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "bit")
                `shouldBe` (Right SqlBitT)
        it "blob" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "blob")
                `shouldBe` (Right SqlLongVarBinaryT)
        it "datetime" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "datetime")
                `shouldBe` (Right SqlTimestampT)
        it "date" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "date")
                `shouldBe` (Right SqlDateT)
        it "decimal" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "decimal")
                `shouldBe` (Right SqlDecimalT)
        it "double" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "double")
                `shouldBe` (Right SqlDoubleT)
        it "enum" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "enum('aaa','bbb')")
                `shouldBe` (Right $ SqlUnknownT "ENUM")
        it "float" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "float")
                `shouldBe` (Right SqlFloatT)
        it "geometry" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "geometry")
                `shouldBe` (Right $ SqlUnknownT "GEOMETRY")
        it "bigint" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "bigint")
                `shouldBe` (Right SqlBigIntT)
        it "mediumblob" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "mediumblob")
                `shouldBe` (Right SqlLongVarBinaryT)
        it "set" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "set('a', 'b')")
                `shouldBe` (Right $ SqlUnknownT "SET")
        it "timestamp" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "timestamp")
                `shouldBe` (Right SqlUTCDateTimeT)
        it "time" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "time")
                `shouldBe` (Right SqlTimeT)
        it "tinyint" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "tinyint")
                `shouldBe` (Right SqlTinyIntT)
        it "tinyblob" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "tinyblob")
                `shouldBe` (Right SqlLongVarBinaryT)
        it "year" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "year")
                `shouldBe` (Right SqlIntegerT)
        it "text" $ do
            (P.parseOnly (columnTypeP <* P.endOfInput) "text")
                `shouldBe` (Right SqlWLongVarCharT)
    describe "columnP" $ do
        it "parse column" $ do
            (P.parseOnly (columnP <* P.endOfInput)
                         "`id` varchar(255) NOT NULL"
                )
                `shouldBe` (Right ("id", makeSqlColDesc SqlVarCharT False))
    describe "pkP" $ do
        it "pk" $ do
            (P.parseOnly (pkP <* P.endOfInput) "PRIMARY KEY (`aa`,`bc`)")
                `shouldBe` (Right ["aa", "bc"])
    describe "otherColDefP" $ do
        it "other col" $ do
            (P.parseOnly
                    (otherColDefP <* P.endOfInput)
                    "KEY `IDX` (`created_at`)"
                )
                `shouldBe` (Right ())
    describe "colDefsP" $ do
        it "col defs" $ do
            (P.parseOnly
                    (colDefsP <* P.endOfInput)
                    (  "  `id` varchar(36) NOT NULL,\n"
                    <> "  `created_at` datetime(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),\n"
                    <> "  PRIMARY KEY (`id`),\n"
                    <> "  KEY `IDX_7e73af02f23a3729b239d7ec8f` (`created_at`)"
                    )
                )
                `shouldBe` (Right
                               ( ["id"]
                               , [ ("id", makeSqlColDesc SqlVarCharT False)
                                 , ( "created_at"
                                   , makeSqlColDesc SqlTimestampT False
                                   )
                                 ]
                               )
                           )
