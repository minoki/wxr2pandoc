{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc where
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Text.Pandoc.Extensions

namespace_dc, namespace_wp, namespace_content :: Maybe T.Text
namespace_dc = Just "http://purl.org/dc/elements/1.1/"
namespace_wp = Just "http://wordpress.org/export/1.2/"
namespace_content = Just "http://purl.org/rss/1.0/modules/content/"

data Post = Post { postTitle :: T.Text
                 , postLink :: T.Text
                 , postPubDate :: Maybe UTCTime
                 , postCreator :: T.Text
                 , postContent :: T.Text
                 , postId :: Maybe Int
                 , postDate :: Maybe LocalTime
                 , postDateGmt :: Maybe LocalTime
                 , postName :: T.Text
                 , postStatus :: T.Text -- publish
                 -- , postType :: T.Text -- post
                 , postCategories :: [T.Text]
                 , postTags :: [T.Text]
                 }
            deriving (Eq, Show)

-- 記事データを output/<slug>.txt および output/<slug>-raw.txt に書き出す。
-- 前者は Markdown に変換したデータ、後者は生データ
renderPostToFile :: Post -> IO ()
renderPostToFile post@Post{..} = do
  let path = "output/" ++ T.unpack postName ++ ".txt"
  let readerOptions :: ReaderOptions
      readerOptions = def { readerExtensions = readerExtensions def <> extensionsFromList [Ext_hard_line_breaks] }
      writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_hard_line_breaks] }
      pandocAction = do doc <- readHtml readerOptions postContent
                        writeMarkdown writerOptions doc
  case flip evalState def $ flip evalStateT def $ runExceptT $ unPandocPure pandocAction of
    Left err -> print err
    Right md -> T.writeFile path $ postTitle <> "\n\n" <> md
  let path_raw = "output/" ++ T.unpack postName ++ "-raw.txt"
  T.writeFile path_raw $ postTitle <> "\n\n" <> postContent

processFile_xmlconduit :: FilePath -> IO ()
processFile_xmlconduit filename = do
  doc <- XC.readFile XC.def filename
  let doc_cursor = XC.fromDocument doc
  let [channel] = doc_cursor XC.$| XC.child >=> XC.element (XC.Name "channel" Nothing Nothing)
      items = channel XC.$| XC.child >=> XC.element (XC.Name "item" Nothing Nothing)
  forM_ items $ \item -> do
    -- element (Name "title" Nothing Nothing) :: Axis
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
    print (title, link_content, pubDate_p, creator, post_name, post_id)
    renderPostToFile Post{ postTitle = title
                         , postLink = link_content
                         , postPubDate = pubDate_p
                         , postCreator = creator
                         , postContent = content_encoded
                         , postId = Nothing
                         , postDate = post_date_p
                         , postDateGmt = post_date_gmt_p
                         , postName = post_name
                         , postStatus = post_status
                         , postCategories = []
                         , postTags = []
                         }
