{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc.Write
  ( HTMLReader(..)
  , parseWpHtmlWith
  , parsePostWith
  , writeCommonMarkFile
  , writePandocJSONFile
  , makeRelativeUrl
  ) where
import qualified Data.ByteString as BS
import           Data.List (stripPrefix)
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Format.ISO8601
import           Network.URI (URI (..), parseAbsoluteURI)
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Definition as P
import qualified Text.Pandoc.Error as P
import           Text.Pandoc.Extensions
import qualified Text.Pandoc.Options as P
import           Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..),
                                      def)
import           Text.Pandoc.Readers.HTML (readHtml)
import           Text.Pandoc.Templates
import qualified Text.Pandoc.Walk as P
import           Text.Pandoc.Writers (writeJSON)
import           Text.Pandoc.Writers.Markdown (writeCommonMark)
-- import           Text.Pandoc.Writers.XML (writeXML) -- pandoc >= 3.8
import           WXR2Pandoc.Filter
import           WXR2Pandoc.ParseHTML
import           WXR2Pandoc.WXR

data HTMLReader = HTMLReaderPandoc | HTMLReaderCustom
                deriving (Eq, Show)

-- | Extract relative URL path from an absolute URL by removing the base URL.
-- If the URL doesn't start with the base URL, returns Nothing.
-- Example: makeRelativeUrl "https://blog.example.com/" "https://blog.example.com/great-article/" = Just "great-article"
makeRelativeUrl :: T.Text -> T.Text -> Maybe T.Text
makeRelativeUrl baseUrl url = do
  baseUri <- parseAbsoluteURI (T.unpack baseUrl)
  targetUri <- parseAbsoluteURI (T.unpack url)
  -- Check if scheme and authority match
  if uriScheme baseUri == uriScheme targetUri && uriAuthority baseUri == uriAuthority targetUri
    then do
      let basePath = uriPath baseUri
          targetPath = uriPath targetUri
      relativePath <- stripPrefix basePath targetPath
      -- Remove trailing slash if present
      let trimmed = case reverse relativePath of
                      '/':rest -> reverse rest
                      _        -> relativePath
      pure $ T.pack trimmed
    else Nothing

parseWpHtmlWith :: HTMLReader -> T.Text -> Either P.PandocError Pandoc
parseWpHtmlWith HTMLReaderPandoc content = fmap wpHtmlFilter . runPure $ readHtml readerOptions content
  where
    readerOptions :: ReaderOptions
    readerOptions = def { readerExtensions = readerExtensions def <> extensionsFromList [Ext_hard_line_breaks, Ext_native_divs, Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash]
                        , readerStripComments = False
                        }
parseWpHtmlWith HTMLReaderCustom content = pure $ parseWpHtml content

parsePostWith :: HTMLReader -> Maybe T.Text -> Post -> Either P.PandocError Pandoc
parsePostWith reader baseUrl Post{..} = do
  Pandoc _ body <- parseWpHtmlWith reader postContent
  let relativeUrl = baseUrl >>= \base -> makeRelativeUrl base postLink
  let meta = Meta $ Map.fromList $
               [("title", MetaString postTitle)
               ,("wp_id", MetaString $ T.pack $ show postId)
               ,("categories", MetaList (map MetaString postCategories))
               ,("tags", MetaList (map MetaString postTags))]
               ++ [("url", MetaString u) | u <- maybeToList relativeUrl]
               ++ [("slug", MetaString postName) | not (T.null postName)]
               ++ [("draft", MetaBool True) | postStatus == "draft"]
               ++ [("date", MetaString $ T.pack $ iso8601Show pd) | pd <- maybeToList postDate]
               ++ [("lastmod", MetaString $ T.pack $ iso8601Show pd) | pd <- maybeToList postModifiedDate]
  pure $ Pandoc meta body

writeCommonMarkFile :: FilePath -> Pandoc -> IO ()
writeCommonMarkFile path doc = do
  let writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash, Ext_yaml_metadata_block]
                          , writerWrapText = P.WrapPreserve
                          }
  content <- P.handleError $ runPure $ do
    tplResult <- runWithPartials $ compileTemplate "" "$if(titleblock)$\n$titleblock$\n\n$endif$\n$body$\n"
    let tpl = case tplResult of
                Left e  -> error e
                Right t -> t
    writeCommonMark (writerOptions { writerTemplate = Just tpl }) $ P.walk unparseShortcode doc
  BS.writeFile path $ T.encodeUtf8 content

writePandocJSONFile :: FilePath -> Pandoc -> IO ()
writePandocJSONFile path doc = do
  let writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash, Ext_yaml_metadata_block]
                          , writerWrapText = P.WrapPreserve
                          }
  content <- P.handleError $ runPure $ writeJSON writerOptions doc
  BS.writeFile path $ T.encodeUtf8 content
