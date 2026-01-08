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
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (EMSMode (AllowEMS), supportedExtendedMainSecret)
import           Network.URI (parseAbsoluteURI, uriPath)
import qualified Options.Applicative as OA
import           System.Directory
import           System.FilePath (takeDirectory, (</>))
import           System.IO
import qualified Text.Pandoc.Error as P
import           WXR2Pandoc.Write
import           WXR2Pandoc.WXR (Item (..), Post (..), postName, processFile)

data AppOptions = AppOptions
  { baseUrl              :: Maybe T.Text
  , outputDir            :: String
  , outputPath           :: Maybe T.Text
  , outputRaw            :: Bool
  -- , outputXml         :: Bool
  , outputJson           :: Bool
  , outputMarkdown       :: Bool
  , htmlReader           :: HTMLReader
  , downloadMedia        :: Bool
  , maxParallelDownloads :: Int
  , maxDownloadFailures  :: Int
  , inputXml             :: String
  }

appOptions :: OA.Parser AppOptions
appOptions = AppOptions
  <$> optional (OA.strOption (OA.long "base-url" <> OA.metavar "URL"))
  <*> OA.strOption (OA.long "output" <> OA.metavar "DIR")
  <*> optional (OA.strOption (OA.long "output-path" <> OA.metavar "TEMPLATE" <> OA.help "Output path template with {url}, {slug}, {year}, {monthnum}, {day}, {post_id}, {ext}"))
  <*> OA.switch (OA.long "raw" <> OA.help "Emit raw HTML")
  -- <*> OA.switch (OA.long "pandoc-xml" <> OA.help "Emit Pandoc XML")
  <*> OA.switch (OA.long "pandoc-json" <> OA.help "Emit Pandoc JSON")
  <*> OA.switch (OA.long "markdown" <> OA.help "Emit Markdown")
  <*> OA.flag HTMLReaderPandoc HTMLReaderCustom (OA.long "custom-parser" <> OA.help "Use custom HTML parser instead of Pandoc")
  <*> OA.switch (OA.long "download-media" <> OA.help "Download media files")
  <*> OA.option OA.auto (OA.long "max-parallel-downloads" <> OA.metavar "N" <> OA.value 5 <> OA.help "Maximum parallel downloads (default: 5)")
  <*> OA.option OA.auto (OA.long "max-download-failures" <> OA.metavar "N" <> OA.value 5 <> OA.help "Abort after N download failures (default: 5)")
  <*> OA.argument OA.str (OA.metavar "FILE.xml")

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
  let opts = OA.info (appOptions <**> OA.helper)
        (OA.fullDesc
         <> OA.progDesc "Convert WordPress Extended RSS (WXR) file to Markdown using Pandoc"
         <> OA.header "wxr2pandoc")
  AppOptions {..} <- OA.execParser opts
  items <- processFile inputXml

  -- Process documents
  forM_ items $ \case
    ItemPost p -> do
      case parsePostWith htmlReader baseUrl p of
        Left e -> hPutStrLn stderr $ "Error: " ++ T.unpack (P.renderError e) ++ " while reading " ++ T.unpack (postName p)
        Right doc -> do
          let getOutputPath ext = case outputPath of
                Just tpl -> outputDir ++ "/" ++ T.unpack (expandOutputPath tpl baseUrl p ext)
                Nothing  -> outputDir ++ "/" ++ show (postId p) ++ "-" ++ T.unpack (postName p) ++ "." ++ T.unpack ext
          let mdPath = getOutputPath "md"
          createDirectoryIfMissing True (takeDirectory mdPath)
          writeCommonMarkFile mdPath doc
          when outputJson $ do
            let jsonPath = getOutputPath "json"
            createDirectoryIfMissing True (takeDirectory jsonPath)
            writePandocJSONFile jsonPath doc
          when outputRaw $ do
            let txtPath = getOutputPath "txt"
            createDirectoryIfMissing True (takeDirectory txtPath)
            BS.writeFile txtPath $ T.encodeUtf8 $ postContent p
    ItemAttachment _ -> pure ()

  -- Download media files if enabled
  when downloadMedia $ do
    let allMediaUrls = concatMap extractMediaUrlsFromItem items
        extractMediaUrlsFromItem (ItemPost _)         = []
        extractMediaUrlsFromItem (ItemAttachment url) = [url]
        uniqueUrls = nub allMediaUrls
    unless (null uniqueUrls) $ do
      hPutStrLn stderr $ "Downloading " ++ show (length uniqueUrls) ++ " media files..."
      ok <- downloadMediaFiles maxParallelDownloads maxDownloadFailures outputDir uniqueUrls
      hPutStrLn stderr $ if ok then "Download complete." else "Download failed."
