{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc.Write
  ( HTMLReader(..)
  , parseWpHtmlWith
  , parsePostWith
  , writeCommonMarkFile
  , renderCommonMark
  , mathDelimitersToEntities
  , writePandocJSONFile
  , makeRelativeUrl
  , siteRelativeUrl
  , relativizeSiteUrls
  , expandOutputPath
  , categoryIndexContent
  ) where
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (LocalTime (..), ZonedTime (..), toGregorian)
import           Data.Time.Format.ISO8601
import           Network.URI (URI (..), URIAuth (..), parseAbsoluteURI,
                                      parseURI)
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
import           Text.Printf (printf)
-- import           Text.Pandoc.Writers.XML (writeXML) -- pandoc >= 3.8
import           WXR2Pandoc.Filter
import           WXR2Pandoc.ParseHTML
import           WXR2Pandoc.WXR

data HTMLReader = HTMLReaderPandoc | HTMLReaderCustom
                deriving (Eq, Show)

-- | Expand output path template with post variables.
-- Supported variables:
--   {url}      - relative URL (from base URL)
--   {slug}     - post name/slug (post ID if empty, e.g. for drafts)
--   {postname} - alias for slug
--   {year}     - 4-digit year
--   {monthnum} - 2-digit month
--   {day}      - 2-digit day
--   {post_id}  - WordPress post ID
--   {ext}      - file extension (md, txt, json)
expandOutputPath :: T.Text -> Maybe T.Text -> Post -> T.Text -> T.Text
expandOutputPath template baseUrl Post{..} ext =
  let relativeUrl = baseUrl >>= \base -> makeRelativeUrl base postLink
      (year, month, day) = fromMaybe ("0000", "00", "00") (postYearMonthDay postDate)
      slug = if T.null postName then T.pack (show postId) else postName
      replacements =
        [ ("{url}", fromMaybe "" relativeUrl)
        , ("{slug}", slug)
        , ("{postname}", slug)
        , ("{year}", T.pack year)
        , ("{monthnum}", T.pack month)
        , ("{day}", T.pack day)
        , ("{post_id}", T.pack $ show postId)
        , ("{ext}", ext)
        ]
      applyReplacements t [] = t
      applyReplacements t ((from, to):rest) = applyReplacements (T.replace from to t) rest
  in applyReplacements template replacements

-- | 4-digit year, 2-digit month and 2-digit day of the post date (in its local time)
postYearMonthDay :: Maybe ZonedTime -> Maybe (String, String, String)
postYearMonthDay date = do
  zt <- date
  let LocalTime d _ = zonedTimeToLocalTime zt
      (y, m, dy) = toGregorian d
  pure (show y, printf "%02d" m, printf "%02d" dy)

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

-- | Root-relative URL like @/2026/07/hello/@ for an absolute URL into the site,
-- along with whether it is a media file (under @wp-content/@).
-- http and https are not distinguished.
-- Example: siteRelativeUrl "https://blog.example.com/" "http://blog.example.com/great-article/#a" = Just ("/great-article/#a", False)
siteRelativeUrl :: T.Text -> T.Text -> Maybe (T.Text, Bool)
siteRelativeUrl baseUrl url = do
  baseUri <- parseAbsoluteURI (T.unpack baseUrl)
  targetUri <- parseURI (T.unpack url) -- parseAbsoluteURI rejects fragments
  guard $ uriScheme targetUri `elem` ["http:", "https:"]
  baseAuth <- uriAuthority baseUri
  targetAuth <- uriAuthority targetUri
  guard $ uriRegName baseAuth == uriRegName targetAuth && uriPort baseAuth == uriPort targetAuth && null (uriUserInfo targetAuth)
  let basePath = case uriPath baseUri of
                   "" -> "/"
                   p  -> p
      targetPath = case uriPath targetUri of
                     "" -> "/"
                     p  -> p
  rest <- stripPrefix basePath targetPath <|> (if targetPath ++ "/" == basePath then Just "" else Nothing)
  pure (T.pack (targetPath ++ uriQuery targetUri ++ uriFragment targetUri), "wp-content/" `isPrefixOf` rest)

-- | Rewrite absolute URLs into the site (see 'siteRelativeUrl') as root-relative URLs,
-- in links, images, attributes in raw HTML and the @images@ metadata.
-- URLs of media files are rewritten only if the first argument is True.
relativizeSiteUrls :: Bool -> T.Text -> Pandoc -> Pandoc
relativizeSiteUrls includeMedia baseUrl (Pandoc (Meta meta) body) =
  Pandoc (Meta (Map.adjust images "images" meta)) (P.walk block (P.walk inline body))
  where
    rewrite url = case siteRelativeUrl baseUrl url of
                    Just (rel, isMedia) | includeMedia || not isMedia -> rel
                    _ -> url
    images (MetaList xs) = MetaList [case x of MetaString u -> MetaString (rewrite u); _ -> x | x <- xs]
    images v = v
    inline (Link attr ils (url, title))  = Link attr ils (rewrite url, title)
    inline (Image attr ils (url, title)) = Image attr ils (rewrite url, title)
    inline (RawInline f@(Format "html") s) = RawInline f (rawHtml s)
    inline x = x
    block (RawBlock f@(Format "html") s) = RawBlock f (rawHtml s)
    block x = x
    -- Rewrite double-quoted attribute values consisting of a URL
    rawHtml = T.intercalate "\"" . zipWith (\i t -> if odd (i :: Int) then rewrite t else t) [0..] . T.splitOn "\""

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
               -- Hugo taxonomies for the date archives (/2026/ and /2026/07/)
               ++ concat [ [("years", MetaList [MetaString (T.pack y)])
                           ,("months", MetaList [MetaString (T.pack (y ++ "/" ++ m))])]
                         | (y, m, _) <- maybeToList (postYearMonthDay postDate) ]
               -- Featured image, shown by the theme above the title
               ++ [("images", MetaList [MetaString t]) | t <- maybeToList postThumbnail]
               ++ [("lastmod", MetaString $ T.pack $ iso8601Show pd) | pd <- maybeToList postModifiedDate]
  pure $ Pandoc meta body

-- | Write a CommonMark file.
-- If the first argument is True, brackets and dollar signs in plain text are
-- written as @&lbrack;@, @&rbrack;@ and @&dollar;@ instead of @\\[@, @\\]@
-- and @$@, which would be taken as math delimiters by Hugo's passthrough extension.
writeCommonMarkFile :: Bool -> FilePath -> Pandoc -> IO ()
writeCommonMarkFile delimiterEntities path doc = do
  content <- P.handleError $ renderCommonMark delimiterEntities doc
  BS.writeFile path $ T.encodeUtf8 content

-- | Render a document as CommonMark with YAML front matter (see 'writeCommonMarkFile')
renderCommonMark :: Bool -> Pandoc -> Either P.PandocError T.Text
renderCommonMark delimiterEntities doc = do
  let writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash, Ext_yaml_metadata_block]
                          , writerWrapText = P.WrapPreserve
                          }
  content <- runPure $ do
    tplResult <- runWithPartials $ compileTemplate "" "$if(titleblock)$\n$titleblock$\n\n$endif$\n$body$\n"
    let tpl = case tplResult of
                Left e  -> error e
                Right t -> t
    writeCommonMark (writerOptions { writerTemplate = Just tpl }) $ rawMathPreamble $ (if delimiterEntities then P.walk mathDelimitersToEntities else id) $ P.walk unparseShortcode doc'
  pure $ insertFrontMatter quotedLines content
  where
    (quotedLines, doc') = takeQuotedStringLists doc

-- | Keys written as lists of quoted strings, like @years: ["2026"]@.
-- Pandoc writes @2026@ without quotes, which YAML reads as a number,
-- and would escape URLs as Markdown text.
quotedStringListKeys :: [T.Text]
quotedStringListKeys = ["years", "months", "images"]

-- | Remove the 'quotedStringListKeys' from the metadata and render them as YAML lines
takeQuotedStringLists :: Pandoc -> ([T.Text], Pandoc)
takeQuotedStringLists (Pandoc (Meta meta) body) =
  ( [ key <> ": [" <> T.intercalate ", " (map quote values) <> "]"
    | key <- quotedStringListKeys, Just (MetaList items) <- [Map.lookup key meta]
    , let values = [v | MetaString v <- items] ]
  , Pandoc (Meta (foldr Map.delete meta quotedStringListKeys)) body )
  where
    -- A JSON string is also a YAML double-quoted string
    quote = T.decodeUtf8 . BS.toStrict . JSON.encode

-- | Append lines at the end of the YAML front matter
insertFrontMatter :: [T.Text] -> T.Text -> T.Text
insertFrontMatter [] content = content
insertFrontMatter newLines content
  | Just rest <- T.stripPrefix "---\n" content
  , (front, end) <- T.breakOn "\n---\n" rest
  , not (T.null end) = "---\n" <> front <> "\n" <> T.intercalate "\n" newLines <> end
  | otherwise = "---\n" <> T.unlines newLines <> "---\n\n" <> content

-- | Replace brackets and dollar signs in Str with HTML entities
mathDelimitersToEntities :: [Inline] -> [Inline]
mathDelimitersToEntities = concatMap go
  where
    go (Str s) = filter (/= Str "") $ map toInline $ T.groupBy (\a b -> not (isDelimiter a) && not (isDelimiter b)) s
    go inl     = [inl]
    isDelimiter c = c == '[' || c == ']' || c == '$'
    toInline "[" = RawInline (Format "html") "&lbrack;"
    toInline "]" = RawInline (Format "html") "&rbrack;"
    toInline "$" = RawInline (Format "html") "&dollar;"
    toInline t   = Str t

-- | Keep the mathPreamble metadata (TeX code) from being escaped as Markdown
rawMathPreamble :: Pandoc -> Pandoc
rawMathPreamble (Pandoc (Meta meta) body) = Pandoc (Meta (Map.adjust toRaw "mathPreamble" meta)) body
  where
    toRaw (MetaString s) = MetaBlocks [RawBlock (Format "commonmark") s]
    toRaw v              = v

writePandocJSONFile :: FilePath -> Pandoc -> IO ()
writePandocJSONFile path doc = do
  let writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash, Ext_yaml_metadata_block]
                          , writerWrapText = P.WrapPreserve
                          }
  content <- P.handleError $ runPure $ writeJSON writerOptions doc
  BS.writeFile path $ T.encodeUtf8 content

-- | Render a Hugo term page (@content/categories/<slug>/_index.md@) for a category.
-- A child category gets @parent@ and a @url@ like WordPress's
-- @/<category base>/<parent>/<child>/@, given the category base (e.g. @category@).
categoryIndexContent :: T.Text -> [Category] -> Category -> T.Text
categoryIndexContent categoryBase allCategories category =
  T.unlines $
    ["+++"
    ,"title = " <> tomlString (categoryName category)]
    ++ ["description = " <> tomlString description | let description = categoryDescription category, not (T.null description)]
    ++ concat [ ["url = " <> tomlString url
                ,"parent = " <> tomlString parent]
              | parent <- maybeToList (categoryParent category) ]
    ++ ["+++"]
  where
    bySlug = Map.fromList [(categorySlug c, c) | c <- allCategories]
    -- Slugs from the root to this category (bounded in case of a cycle)
    ancestors = reverse $ take (length allCategories + 1) $ go category
      where go c = categorySlug c : case categoryParent c of
                     Nothing     -> []
                     Just parent -> maybe [parent] go (Map.lookup parent bySlug)
    url = "/" <> T.intercalate "/" (filter (not . T.null) (T.splitOn "/" categoryBase) ++ ancestors) <> "/"

-- | A TOML basic string
tomlString :: T.Text -> T.Text
tomlString t = "\"" <> T.concatMap escape t <> "\""
  where
    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c | c < ' ' || c == '\DEL' = T.pack (printf "\\u%04X" (fromEnum c))
             | otherwise = T.singleton c
