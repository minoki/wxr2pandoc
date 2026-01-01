{-# LANGUAGE OverloadedStrings #-}
module WXR2Pandoc.ParseHTML
  ( parseWpHtml
  , Token(..)
  , tokenize
  ) where

import           Control.Applicative
import           Data.Char (digitToInt, isDigit, isLetter, isSpace)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Text.HTML.TagSoup (Tag(..), parseTags)
import           Text.Pandoc.Definition
                   (Pandoc(..), Block(..), Inline(..), Format(..), MathType(..),
                    Attr, ListNumberStyle(..), ListNumberDelim(..), Caption(..),
                    TableHead(..), TableBody(..), TableFoot(..), Row(..), Cell(..),
                    Alignment(..), ColWidth(..), RowHeadColumns(..),
                    RowSpan(..), ColSpan(..))

-- | Token types for WordPress HTML
data Token
  = TokText T.Text                            -- ^ Plain text
  | TokTagOpen T.Text [(T.Text, T.Text)]      -- ^ HTML open tag <name attr="val">
  | TokTagClose T.Text                        -- ^ HTML close tag </name>
  | TokTagSelfClose T.Text [(T.Text, T.Text)] -- ^ Self-closing tag <name />
  | TokShortcode T.Text                       -- ^ WordPress shortcode [name attr="val"] (raw string)
  | TokShortcodeClose T.Text                  -- ^ Closing shortcode [/name] (raw string)
  | TokNewlines Int                           -- ^ Consecutive newlines (2+ means paragraph break)
  | TokMathInline T.Text                      -- ^ Inline math \(...\)
  | TokMathDisplay T.Text                     -- ^ Display math \[...\]
  | TokLatexEnv T.Text                        -- ^ LaTeX environment \begin{...}...\end{...}
  deriving (Show, Eq)

-- | Parser for shortcode attributes (internal use)
shortcodeAttrP :: A.Parser ()
shortcodeAttrP = do
  A.skipMany1 A.space
  _ <- A.takeWhile1 isLetter
  _ <- A.char '='
  _ <- A.char '"'
  _ <- A.takeWhile (/= '"')
  _ <- A.char '"'
  pure ()

-- | Parser for opening shortcode [name attr="val"] - preserves raw string
openShortcodeP :: A.Parser Token
openShortcodeP = do
  (src, _) <- A.match $ do
    _ <- A.char '['
    _ <- A.takeWhile1 isLetter
    _ <- many shortcodeAttrP
    A.skipWhile isSpace
    _ <- A.char ']'
    pure ()
  pure $ TokShortcode src

-- | Parser for closing shortcode [/name] - preserves raw string
closeShortcodeP :: A.Parser Token
closeShortcodeP = do
  (src, _) <- A.match $ do
    _ <- A.string "[/"
    _ <- A.takeWhile1 isLetter
    _ <- A.char ']'
    pure ()
  pure $ TokShortcodeClose src

-- | Parser for shortcodes
shortcodeP :: A.Parser Token
shortcodeP = openShortcodeP <|> closeShortcodeP

-- | Parser for inline math \(...\)
mathInlineP :: A.Parser Token
mathInlineP = do
  _ <- A.string "\\("
  content <- A.manyTill A.anyChar (A.string "\\)")
  pure $ TokMathInline (T.pack content)

-- | Parser for display math \[...\]
mathDisplayP :: A.Parser Token
mathDisplayP = do
  _ <- A.string "\\["
  content <- A.manyTill A.anyChar (A.string "\\]")
  pure $ TokMathDisplay (T.pack content)

-- | Parser for LaTeX environment \begin{...}...\end{...}
latexEnvP :: A.Parser Token
latexEnvP = do
  (src, _) <- A.match $ do
    _ <- A.string "\\begin{"
    envName <- A.takeWhile1 (\c -> isLetter c || c == '*')
    _ <- A.char '}'
    _ <- A.manyTill A.anyChar (A.string ("\\end{" <> envName <> "}"))
    pure ()
  pure $ TokLatexEnv src

-- | Parser for consecutive newlines
newlinesP :: A.Parser Token
newlinesP = do
  nl <- A.takeWhile1 (== '\n')
  pure $ TokNewlines (T.length nl)

-- | Parser for separating shortcodes, math, and newlines from text
textTokenP :: A.Parser Token
textTokenP = shortcodeP <|> latexEnvP <|> mathInlineP <|> mathDisplayP <|> newlinesP <|> plainTextP <|> singleCharP
  where
    plainTextP = TokText <$> A.takeWhile1 (\c -> c /= '[' && c /= '\n' && c /= '\\')
    -- Single character fallback when parsing fails
    singleCharP = TokText . T.singleton <$> A.anyChar

-- | Split text into tokens
parseTextTokens :: T.Text -> [Token]
parseTextTokens txt
  | T.null txt = []
  | otherwise = case A.parse (many textTokenP) txt of
      A.Done rest tokens
        | T.null rest -> tokens
        | otherwise -> tokens ++ [TokText rest]
      A.Partial cont -> case cont "" of
        A.Done rest tokens
          | T.null rest -> tokens
          | otherwise -> tokens ++ [TokText rest]
        A.Fail _ _ _ -> [TokText txt]
        A.Partial _ -> [TokText txt]
      A.Fail _ _ _ -> [TokText txt]

-- | Convert HTML tag to tokens
tagToTokens :: Tag T.Text -> [Token]
tagToTokens (TagOpen name attrs)
  | T.null name = []
  | otherwise = [TokTagOpen (T.toLower name) attrs]
tagToTokens (TagClose name)
  | T.null name = []
  | otherwise = [TokTagClose (T.toLower name)]
tagToTokens (TagText txt) = parseTextTokens txt
tagToTokens (TagComment _) = []
tagToTokens (TagWarning _) = []
tagToTokens (TagPosition _ _) = []

-- | Convert WordPress HTML to token list
tokenize :: T.Text -> [Token]
tokenize = concatMap tagToTokens . parseTags

-- | Empty attribute
nullAttr :: Attr
nullAttr = ("", [], [])

-- | Convert attributes to Pandoc Attr
toAttr :: [(T.Text, T.Text)] -> Attr
toAttr attrs =
  let ident = maybe "" id $ lookup "id" attrs
      classes = maybe [] T.words $ lookup "class" attrs
      kvs = [(k, v) | (k, v) <- attrs, k /= "id" && k /= "class"]
  in (ident, classes, kvs)

-- | Check if tag is a block-level element
isBlockTag :: T.Text -> Bool
isBlockTag tag = tag `elem`
  ["p", "div", "ul", "ol", "li", "pre", "blockquote",
   "h1", "h2", "h3", "h4", "h5", "h6",
   "dl", "dt", "dd", "hr", "table", "thead", "tbody", "tfoot",
   "tr", "th", "td", "figure", "figcaption"]

-- | Parse header level from tag name
headerLevel :: T.Text -> Maybe Int
headerLevel tag = do
  ('h', rest) <- T.uncons tag
  (d, rest') <- T.uncons rest
  if T.null rest' && isDigit d
    then Just $ digitToInt d
    else Nothing

-- | Parse tokens into blocks
parseBlocks :: [Token] -> [Block]
parseBlocks [] = []
parseBlocks tokens =
  let (blocks, rest) = parseBlock tokens
  in blocks ++ parseBlocks rest

-- | Parse a single block, returns (blocks, remaining tokens)
parseBlock :: [Token] -> ([Block], [Token])
parseBlock [] = ([], [])
parseBlock (TokTagOpen "p" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "p" rest
  in ([Para $ mergeInlines inlines], rest')
parseBlock (TokTagOpen "div" attrs : rest) =
  let (blocks, rest') = parseBlocksUntil "div" rest
  in ([Div (toAttr attrs) blocks], rest')
parseBlock (TokTagOpen "ul" _ : rest) =
  let (items, rest') = parseListItems "ul" rest
  in ([BulletList items], rest')
parseBlock (TokTagOpen "ol" attrs : rest) =
  let (items, rest') = parseListItems "ol" rest
      startNum = maybe 1 fst $ lookup "start" attrs >>= either (const Nothing) Just . T.decimal
  in ([OrderedList (startNum, DefaultStyle, DefaultDelim) items], rest')
parseBlock (TokTagOpen "pre" attrs : rest) =
  let (content, rest') = collectTextUntil "pre" rest
  in ([CodeBlock (toAttr attrs) content], rest')
parseBlock (TokTagOpen "blockquote" _ : rest) =
  let (blocks, rest') = parseBlocksUntil "blockquote" rest
  in ([BlockQuote blocks], rest')
parseBlock (TokTagOpen tag attrs : rest)
  | Just level <- headerLevel tag =
      let (inlines, rest') = parseInlinesUntil tag rest
      in ([Header level (toAttr attrs) $ mergeInlines inlines], rest')
parseBlock (TokTagOpen "dl" _ : rest) =
  let (items, rest') = parseDefList rest
  in ([DefinitionList items], rest')
parseBlock (TokTagOpen "hr" _ : rest) = ([HorizontalRule], rest)
parseBlock (TokTagClose "hr" : rest) = ([], rest)
parseBlock (TokTagOpen "table" attrs : rest) =
  let (table, rest') = parseTable attrs rest
  in ([table], rest')
parseBlock (TokTagOpen "figure" attrs : rest) =
  let (caption, blocks, rest') = parseFigure rest
  in ([Figure (toAttr attrs) caption blocks], rest')
parseBlock (TokTagOpen "br" _ : rest) = ([], rest)
parseBlock (TokTagClose "br" : rest) = ([], rest)
parseBlock (TokTagClose "img" : rest) = ([], rest)  -- img is self-closing
-- Skip unknown block close tags
parseBlock (TokTagClose tag : rest)
  | isBlockTag tag = ([], rest)
-- Collect inline content into a Para
parseBlock (tok : rest) =
  let (inlines, rest') = collectInlines (tok : rest)
  in if null inlines
     then ([], rest)  -- Skip one token to ensure progress
     else ([Para $ mergeInlines inlines], rest')

-- | Collect inline tokens until we hit a block tag or run out
collectInlines :: [Token] -> ([Inline], [Token])
collectInlines = go []
  where
    go acc [] = (reverse acc, [])
    go acc tokens@(TokTagOpen tag attrs : rest)
      | isBlockTag tag = (reverse acc, tokens)
      | otherwise =
          let (inlines, rest') = parseInline (TokTagOpen tag attrs : rest)
          in go (reverse inlines ++ acc) rest'
    go acc tokens@(TokTagClose tag : rest)
      | isBlockTag tag = (reverse acc, tokens)
      | tag == "img" = go acc rest  -- img is self-closing, skip close tag
      | otherwise = go (tokenToInline (TokTagClose tag) : acc) rest  -- keep unknown close tags
    go acc (TokNewlines n : rest)
      | n >= 2 = (reverse acc, rest)
      | otherwise = go (SoftBreak : acc) rest
    go acc (tok : rest) =
      go (reverse (tokenToInlines tok) ++ acc) rest

-- | Parse inlines until closing tag
parseInlinesUntil :: T.Text -> [Token] -> ([Inline], [Token])
parseInlinesUntil closeTag = go []
  where
    go acc [] = (reverse acc, [])
    go acc (TokTagClose tag : rest)
      | tag == closeTag = (reverse acc, rest)
      | tag == "img" = go acc rest  -- img is self-closing, skip close tag
    go acc (TokTagOpen tag attrs : rest)
      | isBlockTag tag = (reverse acc, TokTagOpen tag attrs : rest)
      | otherwise =
          let (inlines, rest') = parseInline (TokTagOpen tag attrs : rest)
          in go (reverse inlines ++ acc) rest'
    go acc (TokNewlines _ : rest) = go (SoftBreak : acc) rest
    go acc (tok : rest) = go (reverse (tokenToInlines tok) ++ acc) rest

-- | Parse blocks until closing tag
parseBlocksUntil :: T.Text -> [Token] -> ([Block], [Token])
parseBlocksUntil closeTag = go []
  where
    go acc [] = (reverse acc, [])
    go acc (TokTagClose tag : rest)
      | tag == closeTag = (reverse acc, rest)
    go acc tokens =
      let (blocks, rest) = parseBlock tokens
      in go (blocks ++ acc) rest

-- | Collect text content until closing tag
collectTextUntil :: T.Text -> [Token] -> (T.Text, [Token])
collectTextUntil closeTag = go []
  where
    go acc [] = (T.concat $ reverse acc, [])
    go acc (TokTagClose tag : rest)
      | tag == closeTag = (T.concat $ reverse acc, rest)
    go acc (TokText t : rest) = go (t : acc) rest
    go acc (TokNewlines n : rest) = go (T.replicate n "\n" : acc) rest
    go acc (TokTagOpen "code" _ : rest) = go acc rest
    go acc (TokTagClose "code" : rest) = go acc rest
    go acc (_ : rest) = go acc rest

-- | Parse list items
parseListItems :: T.Text -> [Token] -> ([[Block]], [Token])
parseListItems listTag = go []
  where
    go acc [] = (reverse acc, [])
    go acc (TokTagClose tag : rest)
      | tag == listTag = (reverse acc, rest)
    go acc (TokTagOpen "li" _ : rest) =
      let (blocks, rest') = parseListItem rest
      in go (blocks : acc) rest'
    go acc (_ : rest) = go acc rest

    parseListItem :: [Token] -> ([Block], [Token])
    parseListItem = goItem []
      where
        goItem acc [] = (reverse acc, [])
        goItem acc (TokTagClose "li" : rest) = (blocksOrPlain acc, rest)
        goItem acc tokens@(TokTagClose tag : _)
          | tag == listTag = (blocksOrPlain acc, tokens)
        goItem acc tokens@(TokTagOpen "li" _ : _) = (blocksOrPlain acc, tokens)
        goItem acc tokens =
          let (blocks, rest) = parseBlock tokens
          in goItem (blocks ++ acc) rest

        blocksOrPlain [] = []
        blocksOrPlain blocks = reverse blocks

-- | Parse definition list items
parseDefList :: [Token] -> ([([Inline], [[Block]])], [Token])
parseDefList = go []
  where
    go acc [] = (reverse acc, [])
    go acc (TokTagClose "dl" : rest) = (reverse acc, rest)
    go acc (TokTagOpen "dt" _ : rest) =
      let (term, rest') = parseInlinesUntil "dt" rest
          (defs, rest'') = parseDDs rest'
      in go ((mergeInlines term, defs) : acc) rest''
    go acc (_ : rest) = go acc rest

    parseDDs :: [Token] -> ([[Block]], [Token])
    parseDDs = goDDs []
      where
        goDDs acc [] = (reverse acc, [])
        goDDs acc tokens@(TokTagClose "dl" : _) = (reverse acc, tokens)
        goDDs acc tokens@(TokTagOpen "dt" _ : _) = (reverse acc, tokens)
        goDDs acc (TokTagOpen "dd" _ : rest) =
          let (blocks, rest') = parseBlocksUntil "dd" rest
          in goDDs (blocks : acc) rest'
        goDDs acc (_ : rest) = goDDs acc rest

-- | Parse table
parseTable :: [(T.Text, T.Text)] -> [Token] -> (Block, [Token])
parseTable attrs tokens =
  let (headRows, bodyRows, rest) = parseTableContent tokens
      colCount = if null headRows then maybe 0 (length . (\(Row _ cells) -> cells)) (listToMaybe bodyRows)
                 else maybe 0 (length . (\(Row _ cells) -> cells)) (listToMaybe headRows)
      colSpecs = replicate colCount (AlignDefault, ColWidthDefault)
      thead = TableHead nullAttr headRows
      tbody = [TableBody nullAttr (RowHeadColumns 0) [] bodyRows | not (null bodyRows)]
      tfoot = TableFoot nullAttr []
  in (Table (toAttr attrs) (Caption Nothing []) colSpecs thead tbody tfoot, rest)
  where
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

-- | Parse table content (thead, tbody rows)
parseTableContent :: [Token] -> ([Row], [Row], [Token])
parseTableContent = go [] []
  where
    go headRows bodyRows [] = (reverse headRows, reverse bodyRows, [])
    go headRows bodyRows (TokTagClose "table" : rest) = (reverse headRows, reverse bodyRows, rest)
    go headRows bodyRows (TokTagOpen "thead" _ : rest) =
      let (rows, rest') = parseTableRows "thead" rest
      in go (rows ++ headRows) bodyRows rest'
    go headRows bodyRows (TokTagOpen "tbody" _ : rest) =
      let (rows, rest') = parseTableRows "tbody" rest
      in go headRows (rows ++ bodyRows) rest'
    go headRows bodyRows (TokTagOpen "tfoot" _ : rest) =
      let (_, rest') = parseTableRows "tfoot" rest
      in go headRows bodyRows rest'
    go headRows bodyRows (TokTagOpen "tr" attrs : rest) =
      let (row, rest') = parseTableRow attrs rest
      in go headRows (row : bodyRows) rest'
    go headRows bodyRows (_ : rest) = go headRows bodyRows rest

-- | Parse table rows until closing tag
parseTableRows :: T.Text -> [Token] -> ([Row], [Token])
parseTableRows closeTag = go []
  where
    go acc [] = (reverse acc, [])
    go acc (TokTagClose tag : rest)
      | tag == closeTag = (reverse acc, rest)
    go acc (TokTagOpen "tr" attrs : rest) =
      let (row, rest') = parseTableRow attrs rest
      in go (row : acc) rest'
    go acc (_ : rest) = go acc rest

-- | Parse a single table row
parseTableRow :: [(T.Text, T.Text)] -> [Token] -> (Row, [Token])
parseTableRow attrs = go []
  where
    go cells [] = (Row (toAttr attrs) (reverse cells), [])
    go cells (TokTagClose "tr" : rest) = (Row (toAttr attrs) (reverse cells), rest)
    go cells (TokTagOpen tag cellAttrs : rest)
      | tag == "td" || tag == "th" =
          let (blocks, rest') = parseCellContent tag rest
              cell = Cell (toAttr cellAttrs) AlignDefault (RowSpan 1) (ColSpan 1) blocks
          in go (cell : cells) rest'
    go cells (_ : rest) = go cells rest

    parseCellContent :: T.Text -> [Token] -> ([Block], [Token])
    parseCellContent tag = goCell []
      where
        goCell acc [] = (cellBlocks acc, [])
        goCell acc (TokTagClose t : rest)
          | t == tag = (cellBlocks acc, rest)
        goCell acc tokens =
          let (blocks, rest) = parseBlock tokens
          in goCell (blocks ++ acc) rest

        cellBlocks [] = []
        cellBlocks blocks = reverse blocks

-- | Parse figure content
parseFigure :: [Token] -> (Caption, [Block], [Token])
parseFigure = go Nothing []
  where
    go caption blocks [] = (mkCaption caption, reverse blocks, [])
    go caption blocks (TokTagClose "figure" : rest) = (mkCaption caption, reverse blocks, rest)
    go _ blocks (TokTagOpen "figcaption" _ : rest) =
      let (inlines, rest') = parseInlinesUntil "figcaption" rest
      in go (Just $ mergeInlines inlines) blocks rest'
    go caption blocks tokens =
      let (blks, rest) = parseBlock tokens
      in go caption (blks ++ blocks) rest

    mkCaption Nothing = Caption Nothing []
    mkCaption (Just inlines) = Caption Nothing [Plain inlines]

-- | Parse a single inline element
parseInline :: [Token] -> ([Inline], [Token])
parseInline [] = ([], [])
parseInline (TokTagOpen "code" attrs : rest) =
  let (content, rest') = collectCodeContent rest
  in ([Code (toAttr attrs) content], rest')
parseInline (TokTagOpen "a" attrs : rest) =
  let href = maybe "" id $ lookup "href" attrs
      title = maybe "" id $ lookup "title" attrs
      attrs' = filter (\(k, _) -> k /= "href" && k /= "title") attrs
      (inlines, rest') = parseInlinesUntil "a" rest
  in ([Link (toAttr attrs') (mergeInlines inlines) (href, title)], rest')
parseInline (TokTagOpen "strong" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "strong" rest
  in ([Strong $ mergeInlines inlines], rest')
parseInline (TokTagOpen "b" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "b" rest
  in ([Strong $ mergeInlines inlines], rest')
parseInline (TokTagOpen "em" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "em" rest
  in ([Emph $ mergeInlines inlines], rest')
parseInline (TokTagOpen "i" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "i" rest
  in ([Emph $ mergeInlines inlines], rest')
parseInline (TokTagOpen "del" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "del" rest
  in ([Strikeout $ mergeInlines inlines], rest')
parseInline (TokTagOpen "s" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "s" rest
  in ([Strikeout $ mergeInlines inlines], rest')
parseInline (TokTagOpen "img" attrs : rest) =
  let src = maybe "" id $ lookup "src" attrs
      alt = maybe "" id $ lookup "alt" attrs
      title = maybe "" id $ lookup "title" attrs
      attrs' = filter (\(k, _) -> k `notElem` ["src", "alt", "title"]) attrs
  in ([Image (toAttr attrs') [Str alt] (src, title)], rest)
parseInline (TokTagOpen "br" _ : rest) = ([LineBreak], rest)
parseInline (TokTagOpen "span" attrs : rest) =
  let (inlines, rest') = parseInlinesUntil "span" rest
  in ([Span (toAttr attrs) $ mergeInlines inlines], rest')
parseInline (TokTagOpen "sub" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "sub" rest
  in ([Subscript $ mergeInlines inlines], rest')
parseInline (TokTagOpen "sup" _ : rest) =
  let (inlines, rest') = parseInlinesUntil "sup" rest
  in ([Superscript $ mergeInlines inlines], rest')
parseInline (tok : rest) = ([tokenToInline tok], rest)

-- | Collect code content (text only)
collectCodeContent :: [Token] -> (T.Text, [Token])
collectCodeContent = go []
  where
    go acc [] = (T.concat $ reverse acc, [])
    go acc (TokTagClose "code" : rest) = (T.concat $ reverse acc, rest)
    go acc (TokText t : rest) = go (t : acc) rest
    go acc (TokNewlines n : rest) = go (T.replicate n "\n" : acc) rest
    go acc (_ : rest) = go acc rest

-- | Check if character is horizontal whitespace (space or tab)
isHorizWhitespace :: Char -> Bool
isHorizWhitespace c = c == ' ' || c == '\t'

-- | Convert text to list of Str and Space elements
textToInlines :: T.Text -> [Inline]
textToInlines t =
  let chunks = T.groupBy sameType t
      sameType a b = isHorizWhitespace a == isHorizWhitespace b
  in concatMap chunkToInline chunks
  where
    chunkToInline chunk
      | T.null chunk = []
      | isHorizWhitespace (T.head chunk) = [Space]
      | otherwise = [Str chunk]

-- | Convert token to Pandoc Inlines (for non-tag tokens)
tokenToInlines :: Token -> [Inline]
tokenToInlines (TokText t) = textToInlines t
tokenToInlines tok = [tokenToInline tok]

-- | Convert token to Pandoc Inline (for non-tag tokens, non-text)
tokenToInline :: Token -> Inline
tokenToInline (TokText t) = Str t  -- fallback, prefer tokenToInlines
tokenToInline (TokShortcode src) = RawInline (Format "wordpress") src
tokenToInline (TokShortcodeClose src) = RawInline (Format "wordpress") src
tokenToInline (TokTagOpen name attrs) =
  let attrStr = T.concat [" " <> n <> "=\"" <> v <> "\"" | (n, v) <- attrs]
  in RawInline (Format "html") $ "<" <> name <> attrStr <> ">"
tokenToInline (TokTagClose name) =
  RawInline (Format "html") $ "</" <> name <> ">"
tokenToInline (TokTagSelfClose name attrs) =
  let attrStr = T.concat [" " <> n <> "=\"" <> v <> "\"" | (n, v) <- attrs]
  in RawInline (Format "html") $ "<" <> name <> attrStr <> " />"
tokenToInline (TokNewlines _) = SoftBreak
tokenToInline (TokMathInline content) = Math InlineMath content
tokenToInline (TokMathDisplay content) = Math DisplayMath content
tokenToInline (TokLatexEnv src) = RawInline (Format "tex") src

-- | Merge consecutive Str elements and normalize Space
mergeInlines :: [Inline] -> [Inline]
mergeInlines (Str a : Str b : xs) = mergeInlines (Str (a <> b) : xs)
mergeInlines (Space : Space : xs) = mergeInlines (Space : xs)
mergeInlines (Str a : Space : Str b : xs) = mergeInlines (Str (a <> " " <> b) : xs)
mergeInlines (x : xs) = x : mergeInlines xs
mergeInlines [] = []

-- | Convert WordPress HTML string to Pandoc AST
parseWpHtml :: T.Text -> Pandoc
parseWpHtml html =
  let html' = T.filter (/= '\r') html  -- Remove CR from CRLF
      tokens = tokenize html'
      blocks = parseBlocks tokens
  in Pandoc mempty blocks
