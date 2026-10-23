{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
module WXR2Pandoc.WXR
  ( PostType(..)
  , Post(..)
  , Item(..)
  , Category(..)
  , processFile
  , processCategories
  ) where
import           Control.Monad
import           Data.Char
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Time
import           Network.URI (unEscapeString)
import qualified Text.XML as XC
import qualified Text.XML.Cursor as XC

data PostType = TypePost
              | TypePage
              -- attachment?
              deriving (Eq, Show)

data Post = Post { postType         :: PostType
                 , postTitle        :: T.Text
                 , postLink         :: T.Text
                 , postPubDate      :: Maybe UTCTime
                 , postCreator      :: T.Text
                 , postContent      :: T.Text
                 , postId           :: Int
                 , postDate         :: Maybe ZonedTime
                 , postModifiedDate :: Maybe ZonedTime
                 , postName         :: T.Text
                 , postStatus       :: T.Text -- publish, draft, inherit
                 , postCategories   :: [T.Text] -- slugs (nicename)
                 , postTags         :: [T.Text]
                 , postThumbnail    :: Maybe T.Text -- URL of the featured image
                 , postOembeds      :: [T.Text] -- HTML in the oEmbed cache (_oembed_* post meta)
                 }
          deriving (Show)

data Item = ItemPost Post
          | ItemAttachment { attachmentUrl :: T.Text }
          deriving (Show)

-- | A category defined by @<wp:category>@ in the channel
data Category = Category { categorySlug        :: T.Text -- decoded nicename
                         , categoryName        :: T.Text
                         , categoryParent      :: Maybe T.Text -- slug of the parent
                         , categoryDescription :: T.Text
                         }
              deriving (Show)

namespace_dc, namespace_wp, namespace_content :: Maybe T.Text
namespace_dc = Just "http://purl.org/dc/elements/1.1/"
namespace_wp = Just "http://wordpress.org/export/1.2/"
namespace_content = Just "http://purl.org/rss/1.0/modules/content/"

-- | Compute ZonedTime from local time and GMT time.
-- The timezone offset is calculated from the difference between local and GMT.
computeZonedTime :: Maybe LocalTime -> Maybe LocalTime -> Maybe ZonedTime
computeZonedTime (Just local) (Just gmt) =
  let diffSeconds = diffLocalTime local gmt
      diffMinutes = round (diffSeconds / 60)
      tz = minutesToTimeZone diffMinutes
  in Just $ ZonedTime local tz
computeZonedTime (Just local) Nothing =
  Just $ ZonedTime local utc
computeZonedTime Nothing _ = Nothing

-- | Decode a percent-encoded slug like @%e9%89%84%e9%81%93@.
-- 'unEscapeString' also decodes UTF-8.
decodeSlug :: T.Text -> T.Text
decodeSlug = T.pack . unEscapeString . T.unpack

processFile :: FilePath -> IO [Item]
processFile filename = do
  doc <- XC.readFile XC.def filename
  let doc_cursor = XC.fromDocument doc
  let items = doc_cursor XC.$| XC.child >=> XC.element (XC.Name "channel" Nothing Nothing) >=> XC.child >=> XC.element (XC.Name "item" Nothing Nothing)
  {-
  let idToLink :: Map.Map Int T.Text
      idToLink = Map.fromList $ flip concatMap items $ \item -> do
        let link_content = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "link" Nothing Nothing) >=> XC.child >=> XC.content)
        let post_id = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_id" namespace_wp Nothing) >=> XC.child >=> XC.content)
        case T.decimal post_id of
          Left err -> []
          Right (x, _) -> pure (x, link_content)
  -}
  -- Attachment URLs by post ID, to resolve the featured images (_thumbnail_id)
  let attachmentUrls :: Map.Map Int T.Text
      attachmentUrls = Map.fromList $ do
        item <- items
        let field name = mconcat (item XC.$| XC.child >=> XC.element (XC.Name name namespace_wp Nothing) >=> XC.child >=> XC.content)
        guard $ field "post_type" == "attachment"
        Right (attachmentId, _) <- [T.decimal (field "post_id")]
        pure (attachmentId, field "attachment_url")
  fmap concat . forM items $ \item -> do
    -- element (Name "title" Nothing Nothing) :: Axis
    let post_type = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_type" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let title = T.dropWhileEnd isSpace $ T.dropWhile isSpace $ mconcat (item XC.$| XC.child >=> XC.element (XC.Name "title" Nothing Nothing) >=> XC.child >=> XC.content)
    let link_content = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "link" Nothing Nothing) >=> XC.child >=> XC.content)
    let pubDate = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "pubDate" Nothing Nothing) >=> XC.child >=> XC.content) -- rfc822DateFormat %a, %_d %b %Y %H:%M:%S %Z
        pubDate_p = parseTimeM @Maybe @UTCTime True defaultTimeLocale rfc822DateFormat (T.unpack pubDate)
    let creator = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "creator" namespace_dc Nothing) >=> XC.child >=> XC.content)
    let content_encoded = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "encoded" namespace_content Nothing) >=> XC.child >=> XC.content)
    let post_id = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_id" namespace_wp Nothing) >=> XC.child >=> XC.content)
        post_id_i = case T.decimal post_id of
                      Left err     -> error err
                      Right (x, _) -> x
    let post_date = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_date" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_date_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_date)
    let post_date_gmt = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_date_gmt" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_date_gmt_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_date_gmt)
    let post_modified = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_modified" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_modified_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_modified)
    let post_modified_gmt = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_modified_gmt" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_modified_gmt_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_modified_gmt)
    let post_name = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_name" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let post_status = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "status" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let isCategory (XC.Element _ attrs _) = case Map.lookup (XC.Name "domain" Nothing Nothing) attrs of
                                              Just "category" -> True
                                              _               -> False
    let isTag (XC.Element _ attrs _) = case Map.lookup (XC.Name "domain" Nothing Nothing) attrs of
                                              Just "post_tag" -> True
                                              _               -> False
    -- Use the slug (nicename) rather than the display name, falling back to the latter
    let categorySlug c = case XC.attribute (XC.Name "nicename" Nothing Nothing) c of
                           slug:_ -> [decodeSlug slug]
                           []     -> XC.child c >>= XC.content
    let categories = item XC.$| XC.child >=> XC.element (XC.Name "category" Nothing Nothing) >=> XC.checkElement isCategory >=> categorySlug
    let tags = item XC.$| XC.child >=> XC.element (XC.Name "category" Nothing Nothing) >=> XC.checkElement isTag >=> XC.child >=> XC.content
    let postmeta key = item XC.$| XC.child >=> XC.element (XC.Name "postmeta" namespace_wp Nothing)
                                 >=> XC.check (\m -> mconcat (m XC.$/ XC.element (XC.Name "meta_key" namespace_wp Nothing) XC.&/ XC.content) == key)
                                 >=> XC.child >=> XC.element (XC.Name "meta_value" namespace_wp Nothing) >=> XC.child >=> XC.content
    -- WordPress caches oEmbed HTML as _oembed_<hash> (and the time as _oembed_time_<hash>),
    -- or {{unknown}} if the fetch failed
    let oembeds = [ value
                  | m <- item XC.$| XC.child >=> XC.element (XC.Name "postmeta" namespace_wp Nothing)
                  , let key = mconcat (m XC.$/ XC.element (XC.Name "meta_key" namespace_wp Nothing) XC.&/ XC.content)
                  , "_oembed_" `T.isPrefixOf` key, not ("_oembed_time_" `T.isPrefixOf` key)
                  , let value = mconcat (m XC.$/ XC.element (XC.Name "meta_value" namespace_wp Nothing) XC.&/ XC.content)
                  , value /= "{{unknown}}" ]
    let thumbnail = case postmeta "_thumbnail_id" of
                      v:_ | Right (thumbnailId, _) <- T.decimal (T.strip v) -> Map.lookup thumbnailId attachmentUrls
                      _ -> Nothing
    -- <wp:comment>...</wp:comment>
    -- print (title, link_content, pubDate_p, creator, post_name, post_id)
    case post_type of
      "post" -> do
        pure [ItemPost $ Post { postType = TypePost
                              , postTitle = title
                              , postLink = link_content
                              , postPubDate = pubDate_p
                              , postCreator = creator
                              , postContent = content_encoded
                              , postId = post_id_i
                              , postDate = computeZonedTime post_date_p post_date_gmt_p
                              , postModifiedDate = computeZonedTime post_modified_p post_modified_gmt_p
                              , postName = post_name
                              , postStatus = post_status
                              , postCategories = categories
                              , postTags = tags
                              , postThumbnail = thumbnail
                              , postOembeds = oembeds
                              }
             ]
      "page" -> do
        -- putStrLn $ T.unpack post_name ++ " / " ++ T.unpack title
        pure [ItemPost $ Post { postType = TypePage
                              , postTitle = title
                              , postLink = link_content
                              , postPubDate = pubDate_p
                              , postCreator = creator
                              , postContent = content_encoded
                              , postId = post_id_i
                              , postDate = computeZonedTime post_date_p post_date_gmt_p
                              , postModifiedDate = computeZonedTime post_modified_p post_modified_gmt_p
                              , postName = post_name
                              , postStatus = post_status
                              , postCategories = categories
                              , postTags = tags
                              , postThumbnail = thumbnail
                              , postOembeds = oembeds
                              }
             ]
      "attachment" -> do
        let attachment_url = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "attachment_url" namespace_wp Nothing) >=> XC.child >=> XC.content)
        -- putStrLn $ T.unpack attachment_url
        pure [ItemAttachment { attachmentUrl = attachment_url }]
      "nav_menu_item" -> pure []
      "wp_global_styles" -> pure []
      "wp_navigation" -> pure []
      "custom_css" -> pure []
      _ -> do putStrLn $ "Unrecognized post type: " ++ T.unpack post_type
              pure []

processCategories :: FilePath -> IO [Category]
processCategories filename = do
  doc <- XC.readFile XC.def filename
  let doc_cursor = XC.fromDocument doc
  let categories = doc_cursor XC.$| XC.child >=> XC.element (XC.Name "channel" Nothing Nothing) >=> XC.child >=> XC.element (XC.Name "category" namespace_wp Nothing)
  pure $ flip map categories $ \category -> do
    let field name = T.strip $ mconcat (category XC.$| XC.child >=> XC.element (XC.Name name namespace_wp Nothing) >=> XC.child >=> XC.content)
    let parent = field "category_parent"
    Category { categorySlug = decodeSlug (field "category_nicename")
             , categoryName = field "cat_name"
             , categoryParent = if T.null parent then Nothing else Just (decodeSlug parent)
             , categoryDescription = field "category_description"
             }
