{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Options.Applicative as OA
import           System.Directory
import           System.IO
import qualified Text.Pandoc.Error as P
import           WXR2Pandoc.Write
import           WXR2Pandoc.WXR (Item (..), Post (..), processFile)

data AppOptions = AppOptions
  { baseUrl        :: Maybe T.Text
  , outputDir      :: String
  , outputRaw      :: Bool
  -- , outputXml      :: Bool
  , outputJson     :: Bool
  , outputMarkdown :: Bool
  , htmlReader     :: HTMLReader
  , inputXml       :: String
  }

appOptions :: OA.Parser AppOptions
appOptions = AppOptions
  <$> optional (OA.strOption (OA.long "base-url" <> OA.metavar "URL"))
  <*> OA.strOption (OA.long "output" <> OA.metavar "DIR")
  <*> OA.switch (OA.long "raw" <> OA.help "Emit raw HTML")
  -- <*> OA.switch (OA.long "pandoc-xml" <> OA.help "Emit Pandoc XML")
  <*> OA.switch (OA.long "pandoc-json" <> OA.help "Emit Pandoc JSON")
  <*> OA.switch (OA.long "markdown" <> OA.help "Emit Markdown")
  <*> OA.flag HTMLReaderPandoc HTMLReaderCustom (OA.long "custom-parser" <> OA.help "Use custom HTML parser instead of Pandoc")
  <*> OA.argument OA.str (OA.metavar "FILE.xml")

main :: IO ()
main = do
  let opts = OA.info (appOptions <**> OA.helper)
        (OA.fullDesc
         <> OA.progDesc "Convert WordPress Extended RSS (WXR) file to Markdown using Pandoc"
         <> OA.header "wxr2pandoc")
  AppOptions {..} <- OA.execParser opts
  items <- processFile inputXml
  forM_ items $ \item -> case item of
    ItemPost p       -> do
      case parsePostWith htmlReader p of
        Left e -> hPutStrLn stderr $ "Error: " ++ T.unpack (P.renderError e) ++ " while reading " ++ T.unpack (postName p)
        Right doc -> do
          createDirectoryIfMissing True outputDir
          let outputBase = outputDir ++ "/" ++ show (postId p) ++ "-" ++ T.unpack (postName p)
          writeCommonMarkFile (outputBase ++ ".md") doc
          when outputJson $ writePandocJSONFile (outputBase ++ ".json") doc
          when outputRaw $ BS.writeFile (outputBase ++ ".txt") $ T.encodeUtf8 $ postContent p
    ItemAttachment _ -> pure ()
