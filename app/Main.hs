{-
wxr2pandoc
Copyright (C) 2026  ARATA Mizuki

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see
<https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import           Control.Applicative
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.MVar
import           Control.Concurrent.QSem
import           Control.Exception (SomeException, bracket_, try)
import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Default (def)
import           Data.IORef
import           Data.List (nub)
import           Data.Version (showVersion)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (EMSMode (AllowEMS), supportedExtendedMainSecret)
import           Network.URI (parseAbsoluteURI, uriPath)
import qualified Options.Applicative as OA
import           Options.Applicative.Help.Pretty (pretty, vsep)
import           Paths_wxr2pandoc (version)
import           System.Directory
import           System.FilePath (takeDirectory, (</>))
import           System.IO
import qualified Text.Pandoc.Error as P
import           WXR2Pandoc.Filter (hoistMathPreamble)
import           WXR2Pandoc.Write
import           WXR2Pandoc.WXR (Category (..), Item (..), Post (..), postName,
                                 processCategories, processFile)

-- | Subcommand type
data Command
  = ConvertAll ConvertAllOptions
  | DownloadMedia DownloadMediaOptions
  | ExportCategories ExportCategoriesOptions

-- | Options for convert-all subcommand
data ConvertAllOptions = ConvertAllOptions
  { caBaseUrl    :: Maybe T.Text
  , caOutputDir  :: String
  , caOutputPath :: Maybe T.Text
  , caOutputRaw  :: Bool
  , caOutputJson :: Bool
  , caHtmlReader :: HTMLReader
  , caHoistMathPreamble :: Bool
  , caMathDelimiterEntities :: Bool
  , caRelativeMedia :: Bool
  , caEmbedShortcode :: Maybe T.Text
  , caInputXml   :: String
  }

-- | Options for download-media subcommand
data DownloadMediaOptions = DownloadMediaOptions
  { dmOutputDir           :: String
  , dmMaxParallelDownloads :: Int
  , dmMaxDownloadFailures  :: Int
  , dmInputXml            :: String
  }

-- | Options for export-categories subcommand
data ExportCategoriesOptions = ExportCategoriesOptions
  { ecOutputDir    :: String
  , ecCategoryBase :: T.Text
  , ecInputXml     :: String
  }

convertAllOptions :: OA.Parser ConvertAllOptions
convertAllOptions = ConvertAllOptions
  <$> optional (OA.strOption (OA.long "base-url" <> OA.metavar "URL" <> OA.help "Base URL of the blog (e.g. https://blog.example.com/), used to compute relative URLs for {url} and the 'url' metadata, and to rewrite links into the site as root-relative URLs"))
  <*> OA.strOption (OA.long "output" <> OA.metavar "DIR" <> OA.help "Output directory (required)")
  <*> optional (OA.strOption (OA.long "output-path" <> OA.metavar "TEMPLATE" <> OA.help "Output path template relative to DIR, with {url}, {slug}, {year}, {monthnum}, {day}, {post_id}, {ext} (default: {post_id}-{slug}.{ext})"))
  <*> OA.switch (OA.long "raw" <> OA.help "Also emit the raw HTML content as .txt")
  <*> OA.switch (OA.long "pandoc-json" <> OA.help "Also emit the Pandoc AST as .json")
  <*> OA.flag HTMLReaderPandoc HTMLReaderCustom (OA.long "custom-parser" <> OA.help "Use custom HTML parser instead of Pandoc")
  <*> OA.switch (OA.long "hoist-math-preamble" <> OA.help "Move math consisting only of macro definitions (\\newcommand, \\def, \\DeclareMathOperator, etc.) to the 'mathPreamble' metadata")
  <*> OA.switch (OA.long "math-delimiter-entities" <> OA.help "Write brackets and dollar signs in plain text as &lbrack;, &rbrack; and &dollar; instead of \\[, \\] and $, which Hugo's passthrough extension takes as math delimiters")
  <*> OA.switch (OA.long "relative-media" <> OA.help "Also rewrite URLs of media files (under wp-content/) into the site as root-relative URLs (with --base-url), for media placed by 'download-media --output <hugo site>/static'")
  <*> optional (OA.strOption (OA.long "embed-shortcode" <> OA.metavar "NAME" <> OA.help "Replace embed blocks of pages of the site with a Hugo shortcode like {{< NAME \"/2026/07/hello/\" >}} (with --base-url)"))
  <*> OA.argument OA.str (OA.metavar "FILE.xml" <> OA.help "WordPress export file (WXR)")

downloadMediaOptions :: OA.Parser DownloadMediaOptions
downloadMediaOptions = DownloadMediaOptions
  <$> OA.strOption (OA.long "output" <> OA.metavar "DIR" <> OA.help "Output directory; files are saved under DIR by their URL path (required)")
  <*> OA.option OA.auto (OA.long "max-parallel-downloads" <> OA.metavar "N" <> OA.value 5 <> OA.showDefault <> OA.help "Maximum parallel downloads")
  <*> OA.option OA.auto (OA.long "max-download-failures" <> OA.metavar "N" <> OA.value 5 <> OA.showDefault <> OA.help "Abort after N download failures")
  <*> OA.argument OA.str (OA.metavar "FILE.xml" <> OA.help "WordPress export file (WXR)")

exportCategoriesOptions :: OA.Parser ExportCategoriesOptions
exportCategoriesOptions = ExportCategoriesOptions
  <$> OA.strOption (OA.long "output" <> OA.metavar "DIR" <> OA.help "Output directory, e.g. content/categories; each category is written to DIR/<slug>/_index.md (required)")
  <*> OA.strOption (OA.long "category-base" <> OA.metavar "PATH" <> OA.value "category" <> OA.showDefault <> OA.help "Category base of the WordPress permalinks, used for the 'url' of child categories")
  <*> OA.argument OA.str (OA.metavar "FILE.xml" <> OA.help "WordPress export file (WXR)")

-- | Build a footer from lines, keeping line breaks as-is
footerLines :: [String] -> OA.InfoMod a
footerLines = OA.footerDoc . Just . vsep . map pretty

commandParser :: OA.Parser Command
commandParser = OA.hsubparser
  ( OA.command "convert-all"
      (OA.info (ConvertAll <$> convertAllOptions)
               (OA.progDesc "Convert all posts and pages in a WXR file to Markdown files"
                <> footerLines
                     [ "Example:"
                     , "  wxr2pandoc convert-all --output out --base-url https://blog.example.com/ \\"
                     , "    --output-path '{year}/{monthnum}/{slug}.{ext}' WordPress.xml"
                     ]))
  <> OA.command "download-media"
      (OA.info (DownloadMedia <$> downloadMediaOptions)
               (OA.progDesc "Download media files (attachments) listed in a WXR file"
                <> footerLines
                     [ "Example:"
                     , "  wxr2pandoc download-media --output media WordPress.xml"
                     ]))
  <> OA.command "export-categories"
      (OA.info (ExportCategories <$> exportCategoriesOptions)
               (OA.progDesc "Write a Hugo term page (_index.md) for each category in a WXR file"
                <> footerLines
                     [ "Example:"
                     , "  wxr2pandoc export-categories --output content/categories WordPress.xml"
                     ]))
  )

-- | Extract the path from a URL for local file storage
urlToLocalPath :: String -> T.Text -> Maybe FilePath
urlToLocalPath outDir url = do
  uri <- parseAbsoluteURI (T.unpack url)
  let path = uriPath uri
  -- Remove leading slash and ensure path is safe
  let cleanPath = dropWhile (== '/') path
  if null cleanPath
    then Nothing
    else Just (outDir </> cleanPath)

-- | Thread-safe stderr output
logStderr :: MVar () -> String -> IO ()
logStderr lock msg = withMVar lock $ \_ -> hPutStrLn stderr msg

-- | Download a single file from URL to local path
downloadFile :: MVar () -> Manager -> T.Text -> FilePath -> IO ()
downloadFile lock manager url localPath = do
  logStderr lock $ "Downloading: " ++ T.unpack url
  request <- parseRequest (T.unpack url)
  response <- httpLbs request manager
  createDirectoryIfMissing True (takeDirectory localPath)
  LBS.writeFile localPath (responseBody response)

-- | Download multiple files in parallel with a concurrency limit
-- Returns True if completed successfully, False if aborted due to too many failures
downloadMediaFiles :: Int -> Int -> String -> [T.Text] -> IO Bool
downloadMediaFiles maxParallel maxFailures outDir urls = do
  let tlsSettings = TLSSettingsSimple False False False (def { supportedExtendedMainSecret = AllowEMS })
  manager <- newManager $ mkManagerSettings tlsSettings Nothing
  sem <- newQSem maxParallel
  lock <- newMVar ()
  -- Tracks (failure count, aborted flag)
  stateRef <- newIORef (0 :: Int, False)
  let urlsWithPaths = [(url, path) | url <- urls, Just path <- [urlToLocalPath outDir url]]
  forConcurrently_ urlsWithPaths $ \(url, localPath) ->
    bracket_ (waitQSem sem) (signalQSem sem) $ do
      (_, aborted) <- readIORef stateRef
      unless aborted $ do
        result <- try $ downloadFile lock manager url localPath
        case result of
          Left (e :: SomeException) -> do
            logStderr lock $ "Error downloading " ++ T.unpack url ++ ": " ++ show e
            shouldAbort <- atomicModifyIORef' stateRef $ \(count, aborted') ->
              if aborted'
                then ((count, True), False)  -- Already aborted
                else let newCount = count + 1
                     in if newCount >= maxFailures
                        then ((newCount, True), True)   -- This thread triggers abort
                        else ((newCount, False), False) -- Continue
            when shouldAbort $
              logStderr lock $ "Aborting: " ++ show maxFailures ++ " download failures"
          Right () -> pure ()
  (_, aborted) <- readIORef stateRef
  pure (not aborted)

main :: IO ()
main = do
  let opts = OA.info (commandParser <**> OA.helper <**> OA.simpleVersioner (showVersion version))
        (OA.fullDesc
         <> OA.progDesc "Convert a WordPress export file (WordPress eXtended RSS, WXR) to Markdown using Pandoc"
         <> OA.header ("wxr2pandoc " ++ showVersion version)
         <> footerLines
              [ "Run 'wxr2pandoc COMMAND --help' for options of each command."
              ])
      prefs = OA.prefs (OA.showHelpOnEmpty <> OA.showHelpOnError)
  cmd <- OA.customExecParser prefs opts
  case cmd of
    ConvertAll options -> runConvertAll options
    DownloadMedia options -> runDownloadMedia options
    ExportCategories options -> runExportCategories options

-- | Run convert-all subcommand
runConvertAll :: ConvertAllOptions -> IO ()
runConvertAll ConvertAllOptions {..} = do
  items <- processFile caInputXml
  forM_ items $ \case
    ItemPost p -> do
      case parsePostWith caHtmlReader caBaseUrl p of
        Left e -> hPutStrLn stderr $ "Error: " ++ T.unpack (P.renderError e) ++ " while reading " ++ T.unpack (postName p)
        Right doc0 -> do
          let doc = maybe id (relativizeSiteUrls caRelativeMedia) caBaseUrl $
                    maybe id (uncurry siteEmbedsToShortcode) ((,) <$> caEmbedShortcode <*> caBaseUrl) $
                    if caHoistMathPreamble then hoistMathPreamble doc0 else doc0
          let getOutputPath ext = case caOutputPath of
                Just tpl -> caOutputDir ++ "/" ++ T.unpack (expandOutputPath tpl caBaseUrl p ext)
                Nothing  -> caOutputDir ++ "/" ++ show (postId p) ++ "-" ++ T.unpack (postName p) ++ "." ++ T.unpack ext
          let mdPath = getOutputPath "md"
          createDirectoryIfMissing True (takeDirectory mdPath)
          writeCommonMarkFile caMathDelimiterEntities mdPath doc
          when caOutputJson $ do
            let jsonPath = getOutputPath "json"
            createDirectoryIfMissing True (takeDirectory jsonPath)
            writePandocJSONFile jsonPath doc
          when caOutputRaw $ do
            let txtPath = getOutputPath "txt"
            createDirectoryIfMissing True (takeDirectory txtPath)
            BS.writeFile txtPath $ T.encodeUtf8 $ postContent p
    ItemAttachment _ -> pure ()

-- | Run download-media subcommand
runDownloadMedia :: DownloadMediaOptions -> IO ()
runDownloadMedia DownloadMediaOptions {..} = do
  items <- processFile dmInputXml
  let allMediaUrls = concatMap extractMediaUrlsFromItem items
      extractMediaUrlsFromItem (ItemPost _)         = []
      extractMediaUrlsFromItem (ItemAttachment url) = [url]
      uniqueUrls = nub allMediaUrls
  if null uniqueUrls
    then hPutStrLn stderr "No media files to download."
    else do
      hPutStrLn stderr $ "Downloading " ++ show (length uniqueUrls) ++ " media files..."
      ok <- downloadMediaFiles dmMaxParallelDownloads dmMaxDownloadFailures dmOutputDir uniqueUrls
      hPutStrLn stderr $ if ok then "Download complete." else "Download failed."

-- | Run export-categories subcommand
runExportCategories :: ExportCategoriesOptions -> IO ()
runExportCategories ExportCategoriesOptions {..} = do
  categories <- processCategories ecInputXml
  forM_ categories $ \c -> do
    let path = ecOutputDir </> T.unpack (categorySlug c) </> "_index.md"
    createDirectoryIfMissing True (takeDirectory path)
    BS.writeFile path $ T.encodeUtf8 $ categoryIndexContent ecCategoryBase categories c
