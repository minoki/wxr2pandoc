{-# LANGUAGE OverloadedStrings #-}
module WXR2Pandoc.Filter where
import           Control.Applicative
import           Control.Monad (guard)
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.Text as A
import           Data.Char (isDigit, isLetter)
import           Data.List
import           Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Text.Pandoc.Definition as P
import qualified Text.Pandoc.Walk as P

-- | Check if character is valid in shortcode name (letter, digit, or hyphen)
isShortcodeNameChar :: Char -> Bool
isShortcodeNameChar c = isLetter c || isDigit c || c == '-'

splitBySoftBreak :: [Inline] -> [[Inline]]
splitBySoftBreak = go []
  where
    go acc@(_:_) (SoftBreak : xs) = reverse acc : go [] xs
    go [] (SoftBreak : xs)        = go [] xs
    go acc (x : xs)               = go (x : acc) xs
    go acc@(_:_) []               = [reverse acc]
    go [] []                      = []

splitBySoftBreakBlock :: Block -> [Block]
splitBySoftBreakBlock (Para inl) = map Para $ splitBySoftBreak inl
splitBySoftBreakBlock block      = [block]

stripWpComment :: Inline -> [Inline]
stripWpComment (RawInline (P.Format "html") content)
  | T.isPrefixOf "<!-- wp:" content, T.isSuffixOf " -->" content = []
  | T.isPrefixOf "<!-- /wp:" content, T.isSuffixOf " -->" content = []
stripWpComment inl = [inl]

stripClass :: Block -> Block
stripClass (CodeBlock (ident, classes, kv) content) = CodeBlock (ident, filter (\cls -> cls /= "wp-block-preformatted" && cls /= "wp-block-code" && cls /= "wp-block-syntaxhighlighter-code") classes, kv) content
stripClass b = b

attachCodeBlockClass :: [Block] -> [Block]
attachCodeBlockClass (Para [RawInline (P.Format "html") tag] : CodeBlock (ident, classes, kv) content : xs)
  | Just tag' <- T.stripPrefix "<!-- wp:syntaxhighlighter/code " tag
  , Just json <- T.stripSuffix " -->" tag'
  , Just dat <- JSON.decodeStrictText json :: Maybe (Map.Map T.Text T.Text)
  , Just language <- Map.lookup "language" dat = CodeBlock (ident, language : classes, kv) content : attachCodeBlockClass xs
attachCodeBlockClass (x : xs) = x : attachCodeBlockClass xs
attachCodeBlockClass [] = []

-- | Shortcodes registered on the blog.
-- Only these are unescaped by 'escapedShortcodeP', since @[[a]]@ also appears in code.
knownShortcodeNames :: [T.Text]
knownShortcodeNames =
  [ "caption", "code", "sourcecode", "latex", "mathjax", "toc"
  , "TeX-logo", "LaTeX-logo", "XeTeX-logo", "XeLaTeX-logo", "BibTeX-logo", "xypic-logo" ]

-- | Escaped shortcode @[[name attr="val"]]@ or @[[/name]]@, which WordPress displays as @[name attr="val"]@ or @[/name]@.
-- Returns the text with the outer brackets removed.
escapedShortcodeP :: A.Parser T.Text
escapedShortcodeP = do
  _ <- A.char '['
  (inner, _) <- A.match $ do
    _ <- A.char '['
    _ <- optional (A.char '/')
    name <- A.takeWhile1 isShortcodeNameChar
    guard (name `elem` knownShortcodeNames)
    _ <- many (A.space *> A.many1 A.letter *> A.char '=' *> A.char '"' *> A.takeWhile (/= '"') *> A.char '"')
    A.char ']'
  _ <- A.char ']'
  pure inner

-- | Unescape escaped shortcodes in text, e.g. in inline code
unescapeShortcodes :: T.Text -> T.Text
unescapeShortcodes t = case A.parseOnly (many ((escapedShortcodeP <|> (T.singleton <$> A.anyChar))) <* A.endOfInput) t of
  Right chunks -> T.concat chunks
  Left _       -> t

unescapeShortcodeInCode :: Inline -> Inline
unescapeShortcodeInCode (Code attr content) = Code attr (unescapeShortcodes content)
unescapeShortcodeInCode inl = inl

shortcodeP :: A.Parser Inline
shortcodeP = (\(s,_) -> RawInline (P.Format "wordpress") s) <$> A.match (A.char '[' *> A.takeWhile1 isShortcodeNameChar *> many attr *> A.char ']')
  where
    attr = A.space *> A.many1 A.letter *> A.char '=' *> A.char '"' *> many (A.notChar '"') *> A.char '"'

closeShortcodeP :: A.Parser Inline
closeShortcodeP = (\(s,_) -> RawInline (P.Format "wordpress") s) <$> A.match (A.string "[/" *> A.takeWhile1 isShortcodeNameChar *> A.char ']')

latexEnvironmentP :: A.Parser Inline
latexEnvironmentP = (\(s,_) -> RawInline (P.Format "tex") s) <$> A.match p
  where
    p = do
          _ <- A.string "\\begin{"
          name <- A.takeWhile1 (\c -> isLetter c || c == '*')
          _ <- A.char '}'
          A.manyTill A.anyChar (A.string ("\\end{" <> name <> "}"))

strElemP :: A.Parser Inline
strElemP = (Str <$> escapedShortcodeP) <|> shortcodeP <|> closeShortcodeP <|> latexEnvironmentP <|> {- plainStrP <|> -} plainCharP
  where
    -- plainStrP = Str <$> A.takeWhile1 (\c -> c /= '\\' && c /= '[')
    plainCharP = Str . T.singleton <$> A.anyChar

mergeStr :: [Inline] -> [Inline]
mergeStr (Str a : Str b : xs) = mergeStr (Str (a <> b) : xs)
mergeStr (x : xs)             = x : mergeStr xs
mergeStr []                   = []

-- (Str "...[caption") Space (Str "...]...")
-- (Str "...\\begin{align*}...") LineBreak (Str "...\\end{align*}...")
parseStr :: [Inline] -> [Inline]
parseStr (Str s0 : xs0) = parse s0 xs0
  where
    parse "" xs = parseStr xs
    parse s xs  = go (A.parse strElemP s) xs
    go (A.Done rest result) xs                 = result : parse rest xs
    go (A.Partial more) (Str s : xs)           = go (more s) xs
    go (A.Partial more) (P.Space : xs)         = go (more " ") xs
    go (A.Partial more) (LineBreak : xs)       = go (more "\n") xs
    go (A.Partial more) xs                     = go (more "") xs
    go (A.Fail _notConsumed _context message) _ = error $ "fail: " ++ message
parseStr (inl : xs) = inl : parseStr xs
parseStr [] = []

testWpShortcode :: T.Text -> Maybe (T.Text, [(T.Text, T.Text)])
testWpShortcode content = case A.parseOnly p content of
    Left _       -> Nothing
    Right result -> Just result
  where
    p = do _ <- A.char '['
           name <- A.takeWhile1 isShortcodeNameChar
           attrs <- many attr
           _ <- A.char ']'
           pure (name, attrs)
    attr = do _ <- A.space
              attrName <- A.takeWhile1 isLetter
              _ <- A.char '='
              _ <- A.char '"'
              val <- A.takeWhile (/= '"')
              _ <- A.char '"'
              pure (attrName, val)

testWpCloseShortcode :: T.Text -> Maybe T.Text
testWpCloseShortcode content = case A.parseOnly p content of
    Left _       -> Nothing
    Right result -> Just result
  where
    p = do _ <- A.char '['
           _ <- A.char '/'
           name <- A.takeWhile1 isShortcodeNameChar
           _ <- A.char ']'
           pure name

collectCaption :: [Inline] -> ([Inline], [Inline])
collectCaption = go [] []
  where
    stripSpace (P.Space : xs) = stripSpace xs
    stripSpace xs             = xs
    go content caption [] = (reverse content, stripSpace (reverse (stripSpace caption)))
    go content caption (x@(Str _) : xs) = go content (x : caption) xs
    go content caption (x@P.Space : xs) = go content (x : caption) xs
    go content caption (x : xs) = go (x : content) caption xs

processBlockShortcode :: [Block] -> [Block]
processBlockShortcode (Para inlines : blocks0) = goInlines [] inlines blocks0
  where
    takeWpBlock tag = go []
      where go revAcc (RawInline (P.Format "wordpress") tag' : xs)
              | tag' == "[/" <> tag <> "]" = (reverse revAcc, xs)
            go revAcc (x : xs) = go (x : revAcc) xs
            go revAcc [] = ([], reverse revAcc) -- unmatched
    goInlines revAcc (raw@(RawInline (P.Format "wordpress") tag) : xs) blocks
      = case testWpShortcode tag of
          Just ("caption", attrs) -> case takeWpBlock "caption" xs of
            (content, rest) ->
              let attrs' = case Data.List.lookup "align" attrs of
                            Just "center" -> ("", ["aligncenter"], [])
                            _             -> ("", [], [])
                  (content', caption) = collectCaption content
                  fig = Figure attrs' (Caption Nothing [Plain caption]) [Plain content']
              in Para (reverse revAcc) : fig : goInlines [] rest blocks
          Just ("code", attrs) ->
              let attr = case Data.List.lookup "lang" attrs of
                           Just lang -> ("", [lang], [])
                           Nothing   -> ("", [], [])
              in Para (reverse revAcc) : goSourcecode attr [] xs blocks
          Just ("sourcecode", attrs) ->
              let attr = case Data.List.lookup "lang" attrs of
                           Just lang -> ("", [lang], [])
                           Nothing   -> ("", [], [])
              in Para (reverse revAcc) : goSourcecode attr [] xs blocks
          _ -> goInlines (raw : revAcc) xs blocks
    goInlines revAcc (x : xs) blocks = goInlines (x : revAcc) xs blocks
    goInlines revAcc [] blocks = Para (reverse revAcc) : processBlockShortcode blocks
    goSourcecode attr revAcc (RawInline (P.Format "wordpress") tag : xs) blocks
      | tag == "[/code]" || tag == "[/sourcecode]" = CodeBlock attr (T.concat (reverse revAcc)) : processBlockShortcode (Para xs : blocks)
    goSourcecode attr revAcc (Str s : xs) blocks = goSourcecode attr (s : revAcc) xs blocks
    goSourcecode attr revAcc (P.Space : xs) blocks = goSourcecode attr (" " : revAcc) xs blocks
    goSourcecode attr revAcc (LineBreak : xs) blocks = goSourcecode attr ("\n" : revAcc) xs blocks
    goSourcecode attr revAcc xs@(_ : _) blocks = CodeBlock attr (T.concat (reverse revAcc)) : processBlockShortcode (Para xs : blocks)
    goSourcecode attr revAcc@[] [] (Para xs : blocks) = goSourcecode attr revAcc xs blocks -- Immediately after [sourcecode]
    goSourcecode attr revAcc [] (Para xs : blocks) = goSourcecode attr ("\n" : revAcc) xs blocks
    goSourcecode attr revAcc [] blocks = CodeBlock attr (T.concat (reverse revAcc)) : processBlockShortcode blocks
processBlockShortcode (block : blocks) = block : processBlockShortcode blocks
processBlockShortcode [] = []

-- | Convert @[latex]...[/latex]@ (WP LaTeX plugin) into inline math.
-- Left as is if the content contains other than text.
processLatexShortcode :: [Inline] -> [Inline]
processLatexShortcode (open@(RawInline (P.Format "wordpress") tag) : xs)
  | Just ("latex", _) <- testWpShortcode tag
  , Just (content, rest) <- collect [] xs
  = Math InlineMath (T.strip (T.concat content)) : processLatexShortcode rest
  | otherwise = open : processLatexShortcode xs
  where
    collect acc (RawInline (P.Format "wordpress") "[/latex]" : rest) = Just (reverse acc, rest)
    collect acc (Str t : rest)     = collect (t : acc) rest
    collect acc (P.Space : rest)   = collect (" " : acc) rest
    collect acc (SoftBreak : rest) = collect ("\n" : acc) rest
    collect acc (LineBreak : rest) = collect ("\n" : acc) rest
    collect _ _                    = Nothing
processLatexShortcode (x : xs) = x : processLatexShortcode xs
processLatexShortcode [] = []

processInlineShortcode :: Inline -> [Inline]
processInlineShortcode raw@(RawInline (P.Format "wordpress") tag)
  = case testWpShortcode tag of
      Just ("mathjax", _) -> [] -- ignore
      Just ("toc", _) -> [RawInline (P.Format "html") "<!--toc-->"]
      Just ("TeX-logo", _) -> [Span ("", ["TeX-logo"], []) [Str "TeX"]]
      Just ("LaTeX-logo", _) -> [Span ("", ["LaTeX-logo"], []) [Str "LaTeX"]]
      Just ("XeTeX-logo", _) -> [Span ("", ["XeTeX-logo"], []) [Str "XeTeX"]]
      Just ("XeLaTeX-logo", _) -> [Span ("", ["XeLaTeX-logo"], []) [Str "XeLaTeX"]]
      Just ("xypic-logo", _) -> [Span ("", ["xypic-logo"], []) [Str "Xy-pic"]]
      _ -> [raw]
processInlineShortcode inl = [inl]

stripEmptyPara :: [Block] -> [Block]
stripEmptyPara (Para [] : xs)          = stripEmptyPara xs
stripEmptyPara (Para [SoftBreak] : xs) = stripEmptyPara xs
stripEmptyPara (x : xs)                = x : stripEmptyPara xs
stripEmptyPara []                      = []

-- | Replace no-break spaces (U+00A0) in math with ordinary spaces; KaTeX rejects them in strict mode
normalizeMathSpace :: Inline -> Inline
normalizeMathSpace (Math mathType content) = Math mathType (T.replace "\x00A0" " " content)
normalizeMathSpace inl = inl

-- | Collapse a link that consists only of another link to the same URL, e.g. @<a href="u"><a href="u">text</a></a>@
collapseNestedLink :: Inline -> Inline
collapseNestedLink (Link attr [Link _ content (url', _)] target@(url, _))
  | url == url' = Link attr content target
collapseNestedLink inl = inl

wpHtmlFilter :: Pandoc -> Pandoc
wpHtmlFilter = P.walk normalizeMathSpace . P.walk unescapeShortcodeInCode . P.walk stripEmptyPara . P.walk stripClass . P.walk (concatMap processInlineShortcode) . P.walk processLatexShortcode . P.walk processBlockShortcode . P.walk (mergeStr . parseStr) . P.walk (concatMap stripWpComment) . P.walk attachCodeBlockClass . P.walk (concatMap splitBySoftBreakBlock)

wpBasicHtmlFilter :: Pandoc -> Pandoc
wpBasicHtmlFilter = P.walk collapseNestedLink . P.walk normalizeMathSpace . P.walk stripEmptyPara . P.walk stripClass . P.walk (concatMap processInlineShortcode) . P.walk processLatexShortcode . P.walk processBlockShortcode . P.walk (concatMap stripWpComment) . P.walk attachCodeBlockClass

unparseShortcode :: Inline -> Inline
unparseShortcode (RawInline (P.Format "wordpress") content) = Str content
unparseShortcode inl = inl

-- | If the math consists only of macro definitions
-- (@\\newcommand@, @\\renewcommand@, @\\providecommand@, @\\def@, @\\DeclareMathOperator@),
-- return them. @\\require{...}@ (MathJax extensions) is also allowed and dropped.
mathPreambleDefinitions :: T.Text -> Maybe [T.Text]
mathPreambleDefinitions content = case A.parseOnly (A.skipSpace *> many (itemP <* A.skipSpace) <* A.endOfInput) content of
    Right items | defs@(_:_) <- catMaybes items -> Just defs
    _ -> Nothing
  where
    itemP = (Just . fst <$> A.match (newcommandP <|> defP <|> declareMathOperatorP)) <|> (Nothing <$ requireP)
    newcommandP = do
      _ <- A.string "\\newcommand" <|> A.string "\\renewcommand" <|> A.string "\\providecommand"
      _ <- optional (A.char '*')
      A.skipSpace
      _ <- (A.char '{' *> A.skipSpace *> csNameP <* A.skipSpace <* A.char '}') <|> csNameP
      A.skipSpace
      _ <- optional (A.char '[' *> A.takeWhile1 isDigit *> A.char ']' *> A.skipSpace)
      _ <- optional (A.char '[' *> A.takeWhile (/= ']') *> A.char ']' *> A.skipSpace)
      groupP
    defP = do
      _ <- A.string "\\def"
      A.skipSpace
      _ <- csNameP
      _ <- A.takeWhile (\c -> c == '#' || isDigit c)
      groupP
    declareMathOperatorP = do
      _ <- A.string "\\DeclareMathOperator"
      _ <- optional (A.char '*')
      A.skipSpace
      _ <- (A.char '{' *> A.skipSpace *> csNameP <* A.skipSpace <* A.char '}') <|> csNameP
      A.skipSpace
      groupP
    requireP = A.string "\\require" *> A.skipSpace *> groupP
    csNameP = A.char '\\' *> (A.takeWhile1 isLetter <|> (T.singleton <$> A.anyChar))
    groupP = A.char '{' *> A.skipMany (escapedP <|> groupP <|> A.skip (\c -> c /= '{' && c /= '}' && c /= '\\')) <* A.char '}'
    escapedP = A.char '\\' *> A.skip (const True)

-- | Move the math consisting only of macro definitions to the @mathPreamble@ metadata.
-- Hidden spans (@display:none@) that become empty are removed as well.
hoistMathPreamble :: Pandoc -> Pandoc
hoistMathPreamble (Pandoc meta blocks)
  | null defs = Pandoc meta blocks
  | otherwise = Pandoc (Meta (Map.insert "mathPreamble" (MetaString (T.unlines defs)) (unMeta meta)))
                       (P.walk stripEmptyPara $ P.walk stripEmptyHiddenSpan $ P.walk (concatMap removeMath) blocks)
  where
    defs = nub $ P.query collect blocks
    collect (Math _ s) | Just ds <- mathPreambleDefinitions s = ds
    collect _ = []
    removeMath (Math _ s) | Just _ <- mathPreambleDefinitions s = []
    removeMath inl = [inl]
    isHidden style = "display:none" `T.isInfixOf` T.filter (/= ' ') style
    stripEmptyHiddenSpan (RawInline (P.Format "html") open : RawInline (P.Format "html") "</span>" : xs)
      | "<span" `T.isPrefixOf` open, isHidden open = stripEmptyHiddenSpan xs
    stripEmptyHiddenSpan (Span (_, _, kv) [] : xs)
      | Just style <- Data.List.lookup "style" kv, isHidden style = stripEmptyHiddenSpan xs
    stripEmptyHiddenSpan (x : xs) = x : stripEmptyHiddenSpan xs
    stripEmptyHiddenSpan [] = []
