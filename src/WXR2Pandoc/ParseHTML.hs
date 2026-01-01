{-# LANGUAGE OverloadedStrings #-}
module WXR2Pandoc.ParseHTML
  ( parseWpHtml
  , Token(..)
  , tokenize
  ) where

import           Control.Applicative
import           Data.Char (isLetter, isSpace)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import           Text.HTML.TagSoup as TS
import           Text.Pandoc.Definition as P
                   (Pandoc(..), Block(..), Inline(..), Format(..), MathType(..))

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
  | otherwise = [TokTagOpen name attrs]
tagToTokens (TagClose name)
  | T.null name = []
  | otherwise = [TokTagClose name]
tagToTokens (TagText txt) = parseTextTokens txt
tagToTokens (TagComment _) = []
tagToTokens (TagWarning _) = []
tagToTokens (TagPosition _ _) = []

-- | Convert WordPress HTML to token list
tokenize :: T.Text -> [Token]
tokenize = concatMap tagToTokens . TS.parseTags

-- | Convert token to Pandoc Inline
tokenToInline :: Token -> Inline
tokenToInline (TokText t) = Str t
tokenToInline (TokShortcode src) =
  RawInline (P.Format "wordpress") src
tokenToInline (TokShortcodeClose src) =
  RawInline (P.Format "wordpress") src
tokenToInline (TokTagOpen name attrs) =
  let attrStr = T.concat [" " <> n <> "=\"" <> v <> "\"" | (n, v) <- attrs]
  in RawInline (P.Format "html") $ "<" <> name <> attrStr <> ">"
tokenToInline (TokTagClose name) =
  RawInline (P.Format "html") $ "</" <> name <> ">"
tokenToInline (TokTagSelfClose name attrs) =
  let attrStr = T.concat [" " <> n <> "=\"" <> v <> "\"" | (n, v) <- attrs]
  in RawInline (P.Format "html") $ "<" <> name <> attrStr <> " />"
tokenToInline (TokNewlines _) = SoftBreak
tokenToInline (TokMathInline content) = Math InlineMath content
tokenToInline (TokMathDisplay content) = Math DisplayMath content
tokenToInline (TokLatexEnv src) = RawInline (P.Format "tex") src

-- | Split token list into blocks (consecutive newlines as paragraph breaks)
tokensToBlocks :: [Token] -> [Block]
tokensToBlocks = go [] []
  where
    -- Accumulate current inline elements and blocks
    go :: [Inline] -> [Block] -> [Token] -> [Block]
    go inlines blocks [] =
      reverse $ addPara inlines blocks
    go inlines blocks (TokNewlines n : rest)
      | n >= 2 = go [] (addPara inlines blocks) rest
      | otherwise = go (SoftBreak : inlines) blocks rest
    go inlines blocks (tok : rest) =
      go (tokenToInline tok : inlines) blocks rest

    -- Add inlines as a Para block
    addPara :: [Inline] -> [Block] -> [Block]
    addPara [] blocks = blocks
    addPara inlines blocks =
      Para (mergeInlines $ reverse $ dropWhileEnd isSoftBreak $ dropWhile isSoftBreak inlines) : blocks

    isSoftBreak SoftBreak = True
    isSoftBreak _         = False

    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Merge consecutive Str elements
mergeInlines :: [Inline] -> [Inline]
mergeInlines (Str a : Str b : xs) = mergeInlines (Str (a <> b) : xs)
mergeInlines (x : xs) = x : mergeInlines xs
mergeInlines [] = []

-- | Convert WordPress HTML string to Pandoc AST
parseWpHtml :: T.Text -> Pandoc
parseWpHtml html =
  let tokens = tokenize html
      blocks = tokensToBlocks tokens
  in Pandoc mempty blocks
