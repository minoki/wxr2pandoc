{-# LANGUAGE RecordWildCards #-}
module Main (main) where
import           Control.Applicative
import           Control.Monad
import qualified Data.Text as T
import qualified Options.Applicative as OA
import           WXR2Pandoc.ConvertHTML (renderPostToFile)
import           WXR2Pandoc.WXR (processFile)

data AppOptions = AppOptions
  { baseUrl        :: Maybe T.Text
  , outputDir      :: String
  , outputRaw      :: Bool
  , outputXml      :: Bool
  , outputJson     :: Bool
  , outputMarkdown :: Bool
  , inputXml       :: String
  }

appOptions :: OA.Parser AppOptions
appOptions = AppOptions
  <$> optional (OA.strOption (OA.long "base-url" <> OA.metavar "URL"))
  <*> OA.strOption (OA.long "output" <> OA.metavar "DIR")
  <*> OA.switch (OA.long "raw" <> OA.help "Emit raw HTML")
  <*> OA.switch (OA.long "pandoc-xml" <> OA.help "Emit Pandoc XML")
  <*> OA.switch (OA.long "pandoc-json" <> OA.help "Emit Pandoc JSON")
  <*> OA.switch (OA.long "markdown" <> OA.help "Emit Markdown")
  <*> OA.argument OA.str (OA.metavar "FILE.xml")

main :: IO ()
main = do
  let opts = OA.info (appOptions <**> OA.helper)
        (OA.fullDesc
         <> OA.progDesc "Convert WordPress Extended RSS (WXR) file to Markdown using Pandoc"
         <> OA.header "wxr2pandoc")
  AppOptions {..} <- OA.execParser opts
  posts <- processFile inputXml
  forM_ posts $ \post -> renderPostToFile post
