module Database.HDBC.MySQL.Schema
    ( parse
    )
where

import           Database.HDBC.MySQL.Parser     ( schemaParser
                                                , Schema
                                                )
import qualified Data.Attoparsec.Text          as P
import           Data.Text                      ( Text )

parse :: Text -> Either String Schema
parse = P.parseOnly schemaParser
