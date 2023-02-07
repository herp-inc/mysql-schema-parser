module Database.HDBC.MySQL.SchemaSpec
  ( spec
  )
where

import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )
import           Database.HDBC.MySQL.Schema
import           Database.HDBC.ColTypes         ( SqlTypeId(..) )
import           Database.HDBC.MySQL.Parser     ( makeSqlColDesc )
import           Data.Text                      ( pack )
spec :: Spec
spec = describe "Database.HDBC.MySQL.Schema" $ do
  it "wp.sql" $ do
    result <- (parse . pack) <$> readFile "fixtures/wp.sql"
    result
      `shouldBe` (Right
                   [ ( "wp_commentmeta"
                     , ["meta_id"]
                     , [ ("meta_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("comment_id", makeSqlColDesc SqlBigIntT False)
                       , ("meta_key"  , makeSqlColDesc SqlVarCharT True)
                       , ("meta_value", makeSqlColDesc SqlWLongVarCharT True)
                       ]
                     )
                   , ( "wp_comments"
                     , ["comment_ID"]
                     , [ ("comment_ID"     , makeSqlColDesc SqlBigIntT False)
                       , ("comment_post_ID", makeSqlColDesc SqlBigIntT False)
                       , ( "comment_author"
                         , makeSqlColDesc SqlWLongVarCharT False
                         )
                       , ( "comment_author_email"
                         , makeSqlColDesc SqlVarCharT False
                         )
                       , ( "comment_author_url"
                         , makeSqlColDesc SqlVarCharT False
                         )
                       , ("comment_author_IP", makeSqlColDesc SqlVarCharT False)
                       , ("comment_date", makeSqlColDesc SqlTimestampT False)
                       , ( "comment_date_gmt"
                         , makeSqlColDesc SqlTimestampT False
                         )
                       , ( "comment_content"
                         , makeSqlColDesc SqlWLongVarCharT False
                         )
                       , ("comment_karma"   , makeSqlColDesc SqlIntegerT False)
                       , ("comment_approved", makeSqlColDesc SqlVarCharT False)
                       , ("comment_agent"   , makeSqlColDesc SqlVarCharT False)
                       , ("comment_type"    , makeSqlColDesc SqlVarCharT False)
                       , ("comment_parent"  , makeSqlColDesc SqlBigIntT False)
                       , ("user_id"         , makeSqlColDesc SqlBigIntT False)
                       ]
                     )
                   , ( "wp_links"
                     , ["link_id"]
                     , [ ("link_id"         , makeSqlColDesc SqlBigIntT False)
                       , ("link_url"        , makeSqlColDesc SqlVarCharT False)
                       , ("link_name"       , makeSqlColDesc SqlVarCharT False)
                       , ("link_image"      , makeSqlColDesc SqlVarCharT False)
                       , ("link_target"     , makeSqlColDesc SqlVarCharT False)
                       , ("link_description", makeSqlColDesc SqlVarCharT False)
                       , ("link_visible"    , makeSqlColDesc SqlVarCharT False)
                       , ("link_owner"      , makeSqlColDesc SqlBigIntT False)
                       , ("link_rating"     , makeSqlColDesc SqlIntegerT False)
                       , ("link_updated", makeSqlColDesc SqlTimestampT False)
                       , ("link_rel"        , makeSqlColDesc SqlVarCharT False)
                       , ("link_notes", makeSqlColDesc SqlWLongVarCharT False)
                       , ("link_rss"        , makeSqlColDesc SqlVarCharT False)
                       ]
                     )
                   , ( "wp_options"
                     , ["option_id"]
                     , [ ("option_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("option_name" , makeSqlColDesc SqlVarCharT False)
                       , ("option_value", makeSqlColDesc SqlWLongVarCharT False)
                       , ("autoload"    , makeSqlColDesc SqlVarCharT False)
                       ]
                     )
                   , ( "wp_postmeta"
                     , ["meta_id"]
                     , [ ("meta_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("post_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("meta_key"  , makeSqlColDesc SqlVarCharT True)
                       , ("meta_value", makeSqlColDesc SqlWLongVarCharT True)
                       ]
                     )
                   , ( "wp_posts"
                     , ["ID"]
                     , [ ("ID"            , makeSqlColDesc SqlBigIntT False)
                       , ("post_author"   , makeSqlColDesc SqlBigIntT False)
                       , ("post_date"     , makeSqlColDesc SqlTimestampT False)
                       , ("post_date_gmt" , makeSqlColDesc SqlTimestampT False)
                       , ("post_content", makeSqlColDesc SqlWLongVarCharT False)
                       , ("post_title", makeSqlColDesc SqlWLongVarCharT False)
                       , ("post_excerpt", makeSqlColDesc SqlWLongVarCharT False)
                       , ("post_status"   , makeSqlColDesc SqlVarCharT False)
                       , ("comment_status", makeSqlColDesc SqlVarCharT False)
                       , ("ping_status"   , makeSqlColDesc SqlVarCharT False)
                       , ("post_password" , makeSqlColDesc SqlVarCharT False)
                       , ("post_name"     , makeSqlColDesc SqlVarCharT False)
                       , ("to_ping", makeSqlColDesc SqlWLongVarCharT False)
                       , ("pinged", makeSqlColDesc SqlWLongVarCharT False)
                       , ("post_modified" , makeSqlColDesc SqlTimestampT False)
                       , ( "post_modified_gmt"
                         , makeSqlColDesc SqlTimestampT False
                         )
                       , ( "post_content_filtered"
                         , makeSqlColDesc SqlWLongVarCharT False
                         )
                       , ("post_parent"   , makeSqlColDesc SqlBigIntT False)
                       , ("guid"          , makeSqlColDesc SqlVarCharT False)
                       , ("menu_order"    , makeSqlColDesc SqlIntegerT False)
                       , ("post_type"     , makeSqlColDesc SqlVarCharT False)
                       , ("post_mime_type", makeSqlColDesc SqlVarCharT False)
                       , ("comment_count" , makeSqlColDesc SqlBigIntT False)
                       ]
                     )
                   , ( "wp_term_relationships"
                     , ["object_id", "term_taxonomy_id"]
                     , [ ("object_id"       , makeSqlColDesc SqlBigIntT False)
                       , ("term_taxonomy_id", makeSqlColDesc SqlBigIntT False)
                       , ("term_order"      , makeSqlColDesc SqlIntegerT False)
                       ]
                     )
                   , ( "wp_term_taxonomy"
                     , ["term_taxonomy_id"]
                     , [ ("term_taxonomy_id", makeSqlColDesc SqlBigIntT False)
                       , ("term_id"         , makeSqlColDesc SqlBigIntT False)
                       , ("taxonomy"        , makeSqlColDesc SqlVarCharT False)
                       , ("description", makeSqlColDesc SqlWLongVarCharT False)
                       , ("parent"          , makeSqlColDesc SqlBigIntT False)
                       , ("count"           , makeSqlColDesc SqlBigIntT False)
                       ]
                     )
                   , ( "wp_termmeta"
                     , ["meta_id"]
                     , [ ("meta_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("term_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("meta_key"  , makeSqlColDesc SqlVarCharT True)
                       , ("meta_value", makeSqlColDesc SqlWLongVarCharT True)
                       ]
                     )
                   , ( "wp_terms"
                     , ["term_id"]
                     , [ ("term_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("name"      , makeSqlColDesc SqlVarCharT False)
                       , ("slug"      , makeSqlColDesc SqlVarCharT False)
                       , ("term_group", makeSqlColDesc SqlBigIntT False)
                       ]
                     )
                   , ( "wp_usermeta"
                     , ["umeta_id"]
                     , [ ("umeta_id"  , makeSqlColDesc SqlBigIntT False)
                       , ("user_id"   , makeSqlColDesc SqlBigIntT False)
                       , ("meta_key"  , makeSqlColDesc SqlVarCharT True)
                       , ("meta_value", makeSqlColDesc SqlWLongVarCharT True)
                       ]
                     )
                   , ( "wp_users"
                     , ["ID"]
                     , [ ("ID"             , makeSqlColDesc SqlBigIntT False)
                       , ("user_login"     , makeSqlColDesc SqlVarCharT False)
                       , ("user_pass"      , makeSqlColDesc SqlVarCharT False)
                       , ("user_nicename"  , makeSqlColDesc SqlVarCharT False)
                       , ("user_email"     , makeSqlColDesc SqlVarCharT False)
                       , ("user_url"       , makeSqlColDesc SqlVarCharT False)
                       , ("user_registered", makeSqlColDesc SqlTimestampT False)
                       , ( "user_activation_key"
                         , makeSqlColDesc SqlVarCharT False
                         )
                       , ("user_status" , makeSqlColDesc SqlIntegerT False)
                       , ("display_name", makeSqlColDesc SqlVarCharT False)
                       ]
                     )
                   ]
                 )
