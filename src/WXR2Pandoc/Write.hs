{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc.Write
  ( HTMLReader(..)
  , parseWpHtmlWith
  , parsePostWith
  , writeCommonMarkFile
  , renderCommonMark
  , mathDelimitersToEntities
  , unflankedEmphasisToHtml
  , separateCodeBlockFromMore
  , writePandocJSONFile
  , makeRelativeUrl
  , siteRelativeUrl
  , relativizeSiteUrls
  , permalinkPaths
  , siteEmbedsToShortcode
  , youtubeEmbedsToShortcode
  , youtubeVideo
  , tweetEmbedsToHtml
  , tableMathToShortcode
  , tweetUrl
  , expandOutputPath
  , categoryIndexContent
  , urlRewriterContent
  ) where
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Char (isAlphaNum, isDigit, isPunctuation, isSpace,
                                      isSymbol)
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

-- | Root-relative permalinks of published posts and pages by their IDs.
-- Posts whose permalink is itself a query URL like @/?p=ID@ are skipped.
permalinkPaths :: [Post] -> Map.Map Int T.Text
permalinkPaths = Map.fromList . permalinkList

-- | Permalinks of 'permalinkPaths' in the order of the posts
permalinkList :: [Post] -> [(Int, T.Text)]
permalinkList posts =
  [ (postId p, target)
  | p <- posts
  , postStatus p == "publish"
  , Just target <- [permalinkPath (postLink p)] ]
  where
    permalinkPath link = do
      uri <- parseAbsoluteURI (T.unpack link)
      guard $ null (uriQuery uri)
      pure $ T.pack $ case uriPath uri of
                        "" -> "/"
                        p  -> p

-- | Post ID of a root-relative URL like @/?p=ID@ or @/index.php?page_id=ID@ (with an optional fragment)
-- under the base path, and the fragment
queryPostId :: String -> T.Text -> Maybe (Int, T.Text)
queryPostId basePath url = do
  let (beforeFragment, fragment) = T.breakOn "#" url
  (path, query) <- case T.breakOn "?" beforeFragment of
                     (path, q) | Just query <- T.stripPrefix "?" q -> Just (path, query)
                     _ -> Nothing
  guard $ path `elem` [base, base <> "index.php"]
  [n] <- pure [ T.unpack v | param <- T.splitOn "&" query
                           , (k, v0) <- [T.breakOn "=" param]
                           , k `elem` ["p", "page_id"]
                           , Just v <- [T.stripPrefix "=" v0] ]
  guard $ not (null n) && all isDigit n
  pure (read n, fragment)
  where
    base = T.pack $ case basePath of
                      "" -> "/"
                      p  -> p

-- | Rewrite absolute URLs into the site (see 'siteRelativeUrl') as root-relative URLs,
-- in links, images, attributes in raw HTML and the @images@ metadata.
-- URLs of media files are rewritten only if the first argument is True.
-- URLs like @/?p=ID@ are rewritten as the permalinks given by the second argument
-- (see 'permalinkPaths').
relativizeSiteUrls :: Bool -> Map.Map Int T.Text -> T.Text -> Pandoc -> Pandoc
relativizeSiteUrls includeMedia permalinks baseUrl (Pandoc (Meta meta) body) =
  Pandoc (Meta (Map.adjust images "images" meta)) (P.walk block (P.walk inline body))
  where
    basePath = maybe "/" uriPath (parseAbsoluteURI (T.unpack baseUrl))
    rewrite url = case siteRelativeUrl baseUrl url of
                    Just (rel, isMedia) | includeMedia || not isMedia -> toPermalink rel
                    Nothing | "/" `T.isPrefixOf` url, not ("//" `T.isPrefixOf` url) -> toPermalink url
                    _ -> url
    toPermalink rel = case queryPostId basePath rel of
                        Just (n, fragment) | Just target <- Map.lookup n permalinks -> target <> fragment
                        _ -> rel
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

-- | Replace embed blocks (@wp-block-embed@) of pages of the site with a Hugo shortcode
-- like @{{< blogcard "/2026/07/hello/" >}}@, given the shortcode name and the base URL
siteEmbedsToShortcode :: T.Text -> T.Text -> Pandoc -> Pandoc
siteEmbedsToShortcode name baseUrl = P.walk go
  where
    go (Figure (_, classes, _) (Caption Nothing []) [Div (_, ["wp-block-embed__wrapper"], _) [Plain inls]])
      | "wp-block-embed" `elem` classes
      , [Str url] <- filter (`notElem` [SoftBreak, Space, LineBreak]) inls
      , Just (rel, False) <- siteRelativeUrl baseUrl url
      = RawBlock (Format "commonmark") ("{{< " <> name <> " " <> jsonString rel <> " >}}")
    go b = b

-- | Replace YouTube embed blocks (@wp-block-embed@) with Hugo's built-in shortcode
-- like @{{< youtube id="ID" start="10" >}}@.
-- Text of the embed would otherwise be HTML-escaped inside the figure,
-- and Hugo's autolinks take @&amp;@ in the URL literally.
youtubeEmbedsToShortcode :: Pandoc -> Pandoc
youtubeEmbedsToShortcode = P.walk go
  where
    go (Figure attr@(_, classes, _) caption [Div (_, ["wp-block-embed__wrapper"], _) [Plain inls]])
      | "wp-block-embed" `elem` classes
      , [Str url] <- filter (`notElem` [SoftBreak, Space, LineBreak]) inls
      , Just (videoId, start) <- youtubeVideo url
      , let shortcode = RawBlock (Format "commonmark") $
                          "{{< youtube id=" <> jsonString videoId
                          <> maybe "" (\n -> " start=" <> jsonString (T.pack (show n))) start <> " >}}"
      = case caption of
          Caption Nothing [] -> shortcode
          _                  -> Figure attr caption [shortcode]
    go b = b

-- | Replace X (Twitter) embed blocks (@wp-block-embed@) with the tweet HTML in the oEmbed cache
-- of the post (see 'postOembeds').
-- If the tweet is not cached, the block is replaced with Hugo's built-in shortcode
-- like @{{< x user="USER" id="ID" >}}@ if the first argument is True, or kept otherwise.
tweetEmbedsToHtml :: Bool -> [T.Text] -> Pandoc -> Pandoc
tweetEmbedsToHtml shortcode oembeds = P.walk go
  where
    go b@(Figure attr@(_, classes, _) caption [Div (_, ["wp-block-embed__wrapper"], _) [Plain inls]])
      | "wp-block-embed" `elem` classes
      , [Str url] <- filter (`notElem` [SoftBreak, Space, LineBreak]) inls
      , Just (user, tweetId) <- tweetUrl url
      = case filter (isCachedTweet tweetId) oembeds of
          html : _ -> withCaption $ RawBlock (Format "html") (removeBlankLines html)
          [] | shortcode -> withCaption $ RawBlock (Format "commonmark") ("{{< x user=" <> jsonString user <> " id=" <> jsonString tweetId <> " >}}")
             | otherwise -> b
      where
        withCaption block = case caption of
            Caption Nothing [] -> block
            _                  -> Figure attr caption [block]
    go b = b
    -- The cached blockquote links to the tweet like https://twitter.com/USER/status/ID?ref_src=...
    isCachedTweet tweetId html =
      "twitter-tweet" `T.isInfixOf` html
      && any (\(_, rest) -> maybe True (not . isDigit . fst) (T.uncons (T.drop (T.length needle) rest)))
             (T.breakOnAll needle html)
      where needle = "/status/" <> tweetId
    -- A blank line would end the HTML block in CommonMark
    removeBlankLines = T.intercalate "\n" . filter (not . T.all isSpace) . T.lines

-- | Write math in tables as Hugo shortcodes @{{< math >}}TeX{{< /math >}}@
-- (@{{< math display >}}@ for display math).
-- Tables are written as raw HTML, where Hugo's passthrough extension does not find math,
-- while shortcodes are expanded anywhere.
tableMathToShortcode :: Pandoc -> Pandoc
tableMathToShortcode = P.walk table
  where
    table t@(Table {}) = P.walk math t
    table b = b
    math (Math InlineMath s)  = RawInline (Format "html") ("{{< math >}}" <> s <> "{{< /math >}}")
    math (Math DisplayMath s) = RawInline (Format "html") ("{{< math display >}}" <> s <> "{{< /math >}}")
    math x = x

-- | User name and tweet ID of a URL like @https://twitter.com/USER/status/ID@ or @https://x.com/USER/status/ID@
tweetUrl :: T.Text -> Maybe (T.Text, T.Text)
tweetUrl url = do
  uri <- parseAbsoluteURI (T.unpack url)
  guard $ uriScheme uri `elem` ["http:", "https:"]
  auth <- uriAuthority uri
  guard $ T.toLower (T.pack (uriRegName auth)) `elem` ["twitter.com", "www.twitter.com", "mobile.twitter.com", "x.com", "www.x.com"]
  ["", user, status, tweetId] <- pure $ T.splitOn "/" (T.pack (uriPath uri))
  guard $ status `elem` ["status", "statuses"]
  guard $ not (T.null user) && T.all (\c -> isAlphaNum c || c == '_') user
  guard $ not (T.null tweetId) && T.all isDigit tweetId
  pure (user, tweetId)

-- | Video ID and start time (in seconds) of a YouTube URL like
-- @https://www.youtube.com/watch?v=ID&t=10s@ or @https://youtu.be/ID?t=1m30s@
youtubeVideo :: T.Text -> Maybe (T.Text, Maybe Int)
youtubeVideo url = do
  uri <- parseAbsoluteURI (T.unpack url)
  guard $ uriScheme uri `elem` ["http:", "https:"]
  auth <- uriAuthority uri
  let host = T.toLower (T.pack (uriRegName auth))
      path = T.pack (uriPath uri)
      params = [ (k, T.drop 1 v)
               | param <- T.splitOn "&" (T.drop 1 (T.pack (uriQuery uri)))
               , let (k, v) = T.breakOn "=" param ]
  videoId <- if host == "youtu.be"
               then T.stripPrefix "/" path
               else do
                 guard $ host `elem` ["youtube.com", "www.youtube.com", "m.youtube.com"]
                 if path == "/watch"
                   then lookup "v" params
                   else T.stripPrefix "/embed/" path <|> T.stripPrefix "/shorts/" path
  guard $ not (T.null videoId) && T.all (\c -> isAlphaNum c || c == '-' || c == '_') videoId
  let start = do t <- lookup "t" params <|> lookup "start" params
                 n <- seconds t
                 n <$ guard (n > 0)
  pure (videoId, start)
  where
    -- @90@, @90s@ or @1h2m3s@
    seconds t | T.all isDigit t, not (T.null t) = Just (read (T.unpack t))
    seconds t = go 0 t
      where
        go acc rest
          | T.null rest = Just acc
          | otherwise = do
              let (digits, rest') = T.span isDigit rest
              guard $ not (T.null digits)
              (unit, rest'') <- T.uncons rest'
              factor <- lookup unit [('h', 3600), ('m', 60), ('s', 1)]
              go (acc + read (T.unpack digits) * factor) rest''

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
               -- Hugo taxonomies for the date archives (/2026/ and /2026/07/); pages are not listed there
               ++ concat [ [("years", MetaList [MetaString (T.pack y)])
                           ,("months", MetaList [MetaString (T.pack (y ++ "/" ++ m))])]
                         | postType == TypePost
                         , (y, m, _) <- maybeToList (postYearMonthDay postDate) ]
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
    writeCommonMark (writerOptions { writerTemplate = Just tpl }) $ rawMathPreamble $ (if delimiterEntities then P.walk mathDelimitersToEntities else id) $ P.walk unflankedEmphasisToHtml $ P.walk separateCodeBlockFromMore $ P.walk unparseShortcode doc'
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
  ( [ key <> ": [" <> T.intercalate ", " (map jsonString values) <> "]"
    | key <- quotedStringListKeys, Just (MetaList items) <- [Map.lookup key meta]
    , let values = [v | MetaString v <- items] ]
  , Pandoc (Meta (foldr Map.delete meta quotedStringListKeys)) body )

-- | A JSON string, which is also a YAML double-quoted string and a Go (Hugo shortcode) string
jsonString :: T.Text -> T.Text
jsonString = T.decodeUtf8 . BS.toStrict . JSON.encode

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

-- | Character classes that decide whether @*@ can open or close emphasis in CommonMark
data FlankClass = FlankSpace | FlankPunct | FlankOther
                deriving (Eq, Show)

flankClass :: Char -> FlankClass
flankClass c | isSpace c = FlankSpace
             | isPunctuation c || isSymbol c = FlankPunct
             | otherwise = FlankOther

-- | Class of the first character of an inline as written in CommonMark
firstFlankClass :: Inline -> FlankClass
firstFlankClass (Str s) | Just (c, _) <- T.uncons s = flankClass c
firstFlankClass P.Space = FlankSpace
firstFlankClass SoftBreak = FlankSpace
firstFlankClass LineBreak = FlankSpace -- written as two spaces and a newline
firstFlankClass (Span attr (x : _)) | attr == nullAttr = firstFlankClass x
firstFlankClass _ = FlankPunct -- markup such as @*@, @`@, @[@, @\@ or @<@

-- | Class of the last character of an inline as written in CommonMark
lastFlankClass :: Inline -> FlankClass
lastFlankClass (Str s) | Just (_, c) <- T.unsnoc s = flankClass c
lastFlankClass P.Space = FlankSpace
lastFlankClass SoftBreak = FlankSpace
lastFlankClass LineBreak = FlankSpace
lastFlankClass (Span attr xs@(_ : _)) | attr == nullAttr = lastFlankClass (last xs)
lastFlankClass _ = FlankPunct

-- | Write Strong and Emph as HTML tags where @**@ or @*@ would not be recognized,
-- e.g. @、**「強調」**です@: a closing @*@ preceded by punctuation must be followed
-- by whitespace or punctuation (and likewise for an opening one).
-- The start and end of an inline list are treated as punctuation,
-- which behaves the same as whitespace for the outer side of a delimiter.
unflankedEmphasisToHtml :: [Inline] -> [Inline]
unflankedEmphasisToHtml = go FlankPunct
  where
    go before (x : xs) = replace before xs x ++ go (lastFlankClass x) xs
    go _ [] = []
    replace before xs (Strong content) | not (flanked before xs content) = tagged "strong" content
    replace before xs (Emph content) | not (flanked before xs content) = tagged "em" content
    replace _ _ x = [x]
    -- Adjacent emphasis would make a single delimiter run like @****@
    flanked _ (Strong _ : _) _ = False
    flanked _ (Emph _ : _) _ = False
    flanked before xs content = flankedBy before (nextClass xs) content
    nextClass (y : _) = firstFlankClass y
    nextClass []      = FlankPunct
    flankedBy _ _ [] = True
    flankedBy before after content@(c0 : _) =
      let inFirst = firstFlankClass c0
          inLast = lastFlankClass (last content)
      in inFirst /= FlankSpace && (inFirst /= FlankPunct || before /= FlankOther)
         && inLast /= FlankSpace && (inLast /= FlankPunct || after /= FlankOther)
    tagged tag content = [RawInline (Format "html") ("<" <> tag <> ">")] ++ content ++ [RawInline (Format "html") ("</" <> tag <> ">")]

-- | Put an empty HTML comment between the @<!--more-->@ summary divider and
-- a following code block that would be written as an indented code block.
-- Hugo strips the whitespace after the divider, which would turn the code into a paragraph.
separateCodeBlockFromMore :: [Block] -> [Block]
separateCodeBlockFromMore (b : xs@(CodeBlock attr _ : _))
  | attr == nullAttr, endsWithMore b = b : RawBlock (Format "html") "<!-- -->" : separateCodeBlockFromMore xs
  where
    endsWithMore (RawBlock (Format "html") s) = isMore s
    endsWithMore (Para inls)  = endsWithMoreInline inls
    endsWithMore (Plain inls) = endsWithMoreInline inls
    endsWithMore _            = False
    endsWithMoreInline inls = case reverse (filter (`notElem` [P.Space, SoftBreak, LineBreak]) inls) of
                                RawInline (Format "html") s : _ -> isMore s
                                _ -> False
    isMore s = T.strip s == "<!--more-->"
separateCodeBlockFromMore (b : xs) = b : separateCodeBlockFromMore xs
separateCodeBlockFromMore [] = []

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

-- | Render an Apache @.htaccess@ that redirects @/?p=ID@ and @/?page_id=ID@
-- to the permalinks of published posts and pages.
-- Posts whose permalink is itself such a query URL are skipped.
urlRewriterContent :: [Post] -> T.Text
urlRewriterContent posts =
  T.unlines $
    ["# Generated by wxr2pandoc generate-url-rewriter"
    ,"<IfModule mod_rewrite.c>"
    ,"RewriteEngine On"]
    -- NE keeps percent-encoded paths as they are; QSD drops the query string
    ++ concat [ ["RewriteCond %{QUERY_STRING} (?:^|&)(?:p|page_id)=" <> T.pack (show n) <> "(?:&|$)"
                ,"RewriteRule ^(?:index\\.php)?$ " <> target <> " [R=301,L,NE,QSD]"]
              | (n, target) <- permalinkList posts ]
    ++ ["</IfModule>"]

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
