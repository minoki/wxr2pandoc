{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc.ConvertHTML
  ( HTMLReader(..)
  , parseWpHtmlWith
  , parsePostWith
  , writeCommonMarkFile
  , writePandocJSONFile
  ) where
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
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

parseWpHtmlWith :: HTMLReader -> T.Text -> Either P.PandocError Pandoc
parseWpHtmlWith HTMLReaderPandoc content = fmap wpHtmlFilter . runPure $ readHtml readerOptions content
  where
    readerOptions :: ReaderOptions
    readerOptions = def { readerExtensions = readerExtensions def <> extensionsFromList [Ext_hard_line_breaks, Ext_native_divs, Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash]
                        , readerStripComments = False
                        }
parseWpHtmlWith HTMLReaderCustom content = pure $ parseWpHtml content

parsePostWith :: HTMLReader -> Post -> Either P.PandocError Pandoc
parsePostWith reader Post{..} = do
  Pandoc _ body <- parseWpHtmlWith reader postContent
  let meta = Meta $ Map.fromList $
               [("title", MetaString postTitle)]
               ++ [("draft", MetaBool True) | postStatus == "draft"]
               ++ [("categories", MetaList (map MetaString postCategories))]
               ++ [("tags", MetaList (map MetaString postTags))]
               ++ [("date", MetaString $ T.pack $ formatTime defaultTimeLocale rfc822DateFormat pd) | Just pd <- [postDate]]
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
