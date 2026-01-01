{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc.ConvertHTML
  ( HTMLReader(..)
  , renderPostToFile
  ) where
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Definition as P
import           Text.Pandoc.Extensions
import qualified Text.Pandoc.Options as P
import           Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..),
                                      def)
import           Text.Pandoc.Readers.HTML (readHtml)
import           Text.Pandoc.Templates
import           Text.Pandoc.Writers (writeJSON)
import           Text.Pandoc.Writers.Markdown (writeCommonMark)
-- import           Text.Pandoc.Writers.XML (writeXML) -- pandoc >= 3.8
import           WXR2Pandoc.Filter
import           WXR2Pandoc.ParseHTML
import           WXR2Pandoc.WXR

data HTMLReader = HTMLReaderPandoc | HTMLReaderCustom
                deriving (Eq, Show)

-- Writes post data to output/<slug>.md, output/<slug>.json, and output/<slug>-raw.txt
-- The first is Markdown-converted data, the second is Pandoc JSON, the third is raw data
renderPostToFile :: HTMLReader -> Post -> IO ()
renderPostToFile htmlReader Post{..} = do
  let name = if T.null postName
             then show postId
             else T.unpack postName
  let path = "output/" ++ name ++ ".md"
  let meta = Meta $ Map.fromList $
               [("title", MetaString postTitle)]
               ++ [("draft", MetaBool True) | postStatus == "draft"]
               ++ [("categories", MetaList (map MetaString postCategories))]
               ++ [("tags", MetaList (map MetaString postTags))]
               ++ [("date", MetaString $ T.pack $ formatTime defaultTimeLocale rfc822DateFormat pd) | Just pd <- [postDate]]
  let readerOptions :: ReaderOptions
      readerOptions = def { readerExtensions = readerExtensions def <> extensionsFromList [Ext_hard_line_breaks, Ext_native_divs, Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash]
                          , readerStripComments = False
                          }
      writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash, Ext_yaml_metadata_block]
                          , writerWrapText = P.WrapPreserve
                          }
      parseHtml = case htmlReader of
                    HTMLReaderPandoc -> readHtml readerOptions postContent
                    HTMLReaderCustom -> pure $ parseWpHtml postContent
      pandocAction = do doc <- parseHtml
                        tplResult <- runWithPartials $ compileTemplate "" "$if(titleblock)$\n$titleblock$\n\n$endif$\n$body$\n"
                        let tpl = case tplResult of
                                    Left e  -> error e
                                    Right t -> t
                        let Pandoc _ blocks = wpHtmlFilter doc
                        writeCommonMark (writerOptions { writerTemplate = Just tpl }) $ Pandoc meta blocks
  case runPure pandocAction of
    Left err -> print err
    Right md -> T.writeFile path md
  {-
  case runPure (readHtml readerOptions postContent >>= \doc -> writeXML writerOptions (wpHtmlFilter doc)) of
    Left err -> print err
    Right doc -> do
      let path_pd = "output/" ++ name ++ ".xml"
      T.writeFile path_pd doc
  -}
  case runPure (parseHtml >>= \doc -> writeJSON writerOptions (wpHtmlFilter doc)) of
    Left err -> print err
    Right doc -> do
      let path_json = "output/" ++ name ++ ".json"
      T.writeFile path_json doc
  let path_raw = "output/" ++ name ++ "-raw.txt"
  T.writeFile path_raw $ postTitle <> "\n\n" <> postContent
