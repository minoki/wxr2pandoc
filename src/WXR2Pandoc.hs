{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment
import qualified Text.XML as XC
import qualified Text.XML.Cursor as XC
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Data.Time
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Pandoc.Class (PandocPure(..))
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..), def)
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Writers.Markdown (writeCommonMark)
import Text.Pandoc.Writers.XML (writeXML)
import Text.Pandoc.Writers (writeJSON)
import Text.Pandoc.Extensions
import Text.Pandoc.Definition as P
import Text.Pandoc.Templates
import qualified Text.Pandoc.Walk as P
import qualified Data.Aeson as JSON
import qualified Data.Map as Map

namespace_dc, namespace_wp, namespace_content :: Maybe T.Text
namespace_dc = Just "http://purl.org/dc/elements/1.1/"
namespace_wp = Just "http://wordpress.org/export/1.2/"
namespace_content = Just "http://purl.org/rss/1.0/modules/content/"

splitBySoftBreak :: [Inline] -> [[Inline]]
splitBySoftBreak = go []
  where
    go acc@(_:_) (SoftBreak : xs) = reverse acc : go [] xs
    go [] (SoftBreak : xs) = go [] xs
    go acc (x : xs) = go (x : acc) xs
    go acc@(_:_) [] = [reverse acc]
    go [] [] = []

splitBySoftBreakBlock :: Block -> [Block]
splitBySoftBreakBlock (Para inl) = map Para $ splitBySoftBreak inl
splitBySoftBreakBlock block = [block]

stripWpComment :: Inline -> [Inline]
stripWpComment (RawInline (P.Format "html") content)
  | T.isPrefixOf "<!-- wp:" content, T.isSuffixOf " -->" content = []
  | T.isPrefixOf "<!-- /wp:" content, T.isSuffixOf " -->" content = []
stripWpComment inl = [inl]

stripClass :: Block -> Block
stripClass (CodeBlock (ident, classes, kv) content) = CodeBlock (ident, filter (/= "wp-block-preformatted") classes, kv) content
stripClass b = b

attachCodeBlockClass :: [Block] -> [Block]
attachCodeBlockClass (Para [RawInline (P.Format "html") tag] : CodeBlock (ident, classes, kv) content : xs)
  | Just tag' <- T.stripPrefix "<!-- wp:syntaxhighlighter/code " tag
  , Just json <- T.stripSuffix " -->" tag'
  , Just dat <- JSON.decodeStrictText json :: Maybe (Map.Map T.Text T.Text)
  , Just language <- Map.lookup "language" dat = CodeBlock (ident, language : classes, kv) content : attachCodeBlockClass xs
attachCodeBlockClass (x : xs) = x : attachCodeBlockClass xs
attachCodeBlockClass [] = []

data Post = Post { postTitle :: T.Text
                 , postLink :: T.Text
                 , postPubDate :: Maybe UTCTime
                 , postCreator :: T.Text
                 , postContent :: T.Text
                 , postId :: Int
                 , postDate :: Maybe LocalTime
                 , postDateGmt :: Maybe LocalTime
                 , postName :: T.Text
                 , postStatus :: T.Text -- publish, draft, inherit
                 -- , postType :: T.Text -- post, attachment
                 , postCategories :: [T.Text]
                 , postTags :: [T.Text]
                 }
            deriving (Eq, Show)

-- 記事データを output/<slug>.txt および output/<slug>-raw.txt に書き出す。
-- 前者は Markdown に変換したデータ、後者は生データ
renderPostToFile :: Post -> IO ()
renderPostToFile post@Post{..} = do
  let name = if T.null postName
             then show postId
             else T.unpack postName
  let path = "output/" ++ name ++ ".md"
  let meta = Meta $ Map.fromList $
               [("title", MetaString postTitle)]
               ++ [("draft", MetaBool True) | postStatus == "draft"]
               ++ [("date", MetaString $ T.pack $ formatTime defaultTimeLocale rfc822DateFormat pd) | Just pd <- [postDate]]
  let readerOptions :: ReaderOptions
      readerOptions = def { readerExtensions = readerExtensions def <> extensionsFromList [Ext_hard_line_breaks, Ext_raw_html, Ext_tex_math_single_backslash]
                          , readerStripComments = False
                          }
      writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_hard_line_breaks, Ext_raw_html, Ext_tex_math_single_backslash, Ext_yaml_metadata_block]
                          }
      pandocAction = do doc <- readHtml readerOptions postContent
                        -- tpl <- getDefaultTemplate "commonmark" >>= compileDefaultTemplate
                        let Pandoc _ blocks = P.walk stripClass $ P.walk (concatMap stripWpComment) $ P.walk attachCodeBlockClass $ P.walk (concatMap splitBySoftBreakBlock) doc
                        writeCommonMark (writerOptions {- writerTemplate = Just tpl -}) $ Pandoc meta blocks
  case flip evalState def $ flip evalStateT def $ runExceptT $ unPandocPure pandocAction of
    Left err -> print err
    Right md -> T.writeFile path $ postTitle <> "\n\n" <> md
  case flip evalState def $ flip evalStateT def $ runExceptT $ unPandocPure (readHtml readerOptions postContent >>= writeXML writerOptions) of
    Left err -> print err
    Right pd -> do
      let path_pd = "output/" ++ name ++ "-pandoc.txt"
      T.writeFile path_pd $ postTitle <> "\n\n" <> pd
  case flip evalState def $ flip evalStateT def $ runExceptT $ unPandocPure (readHtml readerOptions postContent >>= writeJSON writerOptions) of
    Left err -> print err
    Right pd -> do
      let path_pd = "output/" ++ name ++ "-json.txt"
      T.writeFile path_pd $ pd
  let path_raw = "output/" ++ name ++ "-raw.txt"
  T.writeFile path_raw $ postTitle <> "\n\n" <> postContent

processFile_xmlconduit :: FilePath -> IO ()
processFile_xmlconduit filename = do
  doc <- XC.readFile XC.def filename
  let doc_cursor = XC.fromDocument doc
  let [channel] = doc_cursor XC.$| XC.child >=> XC.element (XC.Name "channel" Nothing Nothing)
      items = channel XC.$| XC.child >=> XC.element (XC.Name "item" Nothing Nothing)
  forM_ items $ \item -> do
    -- element (Name "title" Nothing Nothing) :: Axis
    let post_type = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_type" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let title = T.dropWhileEnd isSpace $ T.dropWhile isSpace $ mconcat (item XC.$| XC.child >=> XC.element (XC.Name "title" Nothing Nothing) >=> XC.child >=> XC.content)
    let link_content = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "link" Nothing Nothing) >=> XC.child >=> XC.content)
    let pubDate = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "pubDate" Nothing Nothing) >=> XC.child >=> XC.content) -- rfc822DateFormat %a, %_d %b %Y %H:%M:%S %Z
        pubDate_p = parseTimeM @Maybe @UTCTime True defaultTimeLocale rfc822DateFormat (T.unpack pubDate)
    let creator = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "creator" namespace_dc Nothing) >=> XC.child >=> XC.content)
    let content_encoded = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "encoded" namespace_content Nothing) >=> XC.child >=> XC.content)
    let post_id = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_id" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let post_date = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_date" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_date_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_date)
    let post_date_gmt = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_date_gmt" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_date_gmt_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_date_gmt)
    let post_name = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_name" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let post_status = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "status" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let category = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "category" Nothing Nothing) >=> XC.child >=> XC.content)
    -- print (title, link_content, pubDate_p, creator, post_name, post_id)
    case post_type of
      "post" -> do
        renderPostToFile Post{ postTitle = title
                             , postLink = link_content
                             , postPubDate = pubDate_p
                             , postCreator = creator
                             , postContent = content_encoded
                             , postId = case T.decimal post_id of
                                          Left err -> error err
                                          Right (x, _) -> x
                             , postDate = post_date_p
                             , postDateGmt = post_date_gmt_p
                             , postName = post_name
                             , postStatus = post_status
                             , postCategories = []
                             , postTags = []
                             }
      "page" -> putStrLn $ T.unpack post_name ++ " / " ++ T.unpack title
      "attachment" -> do
        let attachment_url = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "attachment_url" namespace_wp Nothing) >=> XC.child >=> XC.content)
        -- putStrLn $ T.unpack attachment_url
        pure ()
      "nav_menu_item" -> pure ()
      "wp_global_styles" -> pure ()
      "wp_navigation" -> pure ()
      "custom_css" -> pure ()
      _ -> putStrLn $ "Unrecognized post type: " ++ T.unpack post_type
