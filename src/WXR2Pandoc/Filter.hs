{-# LANGUAGE OverloadedStrings #-}
module WXR2Pandoc.Filter where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import System.Environment
import qualified Text.XML as XC
import qualified Text.XML.Cursor as XC
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Data.Time
import Control.Monad.State.Strict
import Control.Monad.Except
import Text.Pandoc.Class (PandocPure(..), runPure)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..), def)
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Writers.Markdown (writeCommonMark)
import Text.Pandoc.Writers.XML (writeXML)
import Text.Pandoc.Writers (writeJSON)
import Text.Pandoc.Extensions
import Text.Pandoc.Definition as P
import Text.Pandoc.Templates
import qualified Text.Pandoc.Walk as P
import qualified Text.Pandoc.Options as P
import qualified Data.Aeson as JSON
import qualified Data.Map as Map
import qualified Data.Attoparsec.Text as A
import Control.Applicative

splitBySoftBreak :: [Inline] -> [[Inline]]
splitBySoftBreak = go []
  where
    go acc@(_:_) (SoftBreak : xs) = reverse acc : go [] xs
    go [] (SoftBreak : xs) = go [] xs
    go acc (x : xs) = go (x : acc) xs
    go acc@(_:_) [] = [reverse acc]
    go [] [] = []

splitBySoftBreakBlock :: Block -> [Block]
splitBySoftBreakBlock (Para inl) = map Para $ splitBySoftBreak inl
splitBySoftBreakBlock block = [block]

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

shortcodeP :: A.Parser Inline
shortcodeP = (\(s,_) -> RawInline (P.Format "wordpress") s) <$> A.match (A.char '[' *> A.many1 A.letter *> many attr *> A.char ']')
  where
    attr = A.space *> A.many1 A.letter *> A.char '=' *> A.char '"' *> many (A.notChar '"') *> A.char '"'

closeShortcodeP :: A.Parser Inline
closeShortcodeP = (\(s,_) -> RawInline (P.Format "wordpress") s) <$> A.match (A.string "[/" *> A.many1 A.letter *> A.char ']')

latexEnvironmentP :: A.Parser Inline
latexEnvironmentP = (\(s,_) -> RawInline (P.Format "tex") s) <$> A.match p
  where
    p = do
          A.string "\\begin{"
          name <- A.takeWhile1 (\c -> isLetter c || c == '*')
          A.char '}'
          A.manyTill A.anyChar (A.string ("\\end{" <> name <> "}"))

strElemP :: A.Parser Inline
strElemP = shortcodeP <|> closeShortcodeP <|> latexEnvironmentP <|> {- plainStrP <|> -} plainCharP
  where
    -- plainStrP = Str <$> A.takeWhile1 (\c -> c /= '\\' && c /= '[')
    plainCharP = Str . T.singleton <$> A.anyChar

mergeStr :: [Inline] -> [Inline]
mergeStr (Str a : Str b : xs) = mergeStr (Str (a <> b) : xs)
mergeStr (x : xs) = x : mergeStr xs
mergeStr [] = []

-- (Str "...[caption") Space (Str "...]...")
-- (Str "...\\begin{align*}...") LineBreak (Str "...\\end{align*}...")
parseStr :: [Inline] -> [Inline]
parseStr (Str s : xs) = parse s xs
  where
    parse "" xs = parseStr xs
    parse s xs = go (A.parse strElemP s) xs
    go (A.Done rest result) xs = result : parse rest xs
    go (A.Partial more) (Str s : xs) = go (more s) xs
    go (A.Partial more) (P.Space : xs) = go (more " ") xs
    go (A.Partial more) (LineBreak : xs) = go (more "\n") xs
    go (A.Partial more) xs = go (more "") xs
    go (A.Fail notConsumed context message) xs = error $ "fail: " ++ message
parseStr (inl : xs) = inl : parseStr xs
parseStr [] = []

testWpShortcode :: T.Text -> Maybe (T.Text, [(T.Text, T.Text)])
testWpShortcode content = case A.parseOnly p content of
    Left _ -> Nothing
    Right result -> Just result
  where
    p = do A.char '['
           name <- A.takeWhile1 isLetter
           attrs <- many attr
           A.char ']'
           pure (name, attrs)
    attr = do A.space
              attrName <- A.takeWhile1 isLetter
              A.char '='
              A.char '"'
              val <- A.takeWhile (/= '"')
              A.char '"'
              pure (attrName, val)

testWpCloseShortcode :: T.Text -> Maybe T.Text
testWpCloseShortcode content = case A.parseOnly p content of
    Left _ -> Nothing
    Right result -> Just result
  where
    p = do A.char '['
           A.char '/'
           name <- A.takeWhile1 isLetter
           A.char ']'
           pure name

collectCaption :: [Inline] -> ([Inline], [Inline])
collectCaption = go [] []
  where
    stripSpace (P.Space : xs) = stripSpace xs
    stripSpace xs = xs
    go content caption [] = (reverse content, stripSpace (reverse (stripSpace caption)))
    go content caption (x@(Str _) : xs) = go content (x : caption) xs
    go content caption (x@P.Space : xs) = go content (x : caption) xs
    go content caption (x : xs) = go (x : content) caption xs

processWpShortcode :: [Block] -> [Block]
processWpShortcode (Para inlines : blocks) = goInlines [] inlines blocks
  where
    takeWpBlock tag = go []
      where go revAcc (raw@(RawInline (P.Format "wordpress") tag') : xs)
              | tag' == "[/" <> tag <> "]" = (reverse revAcc, xs)
            go revAcc (x : xs) = go (x : revAcc) xs
            go revAcc [] = ([], reverse revAcc) -- unmatched
    goInlines revAcc (raw@(RawInline (P.Format "wordpress") tag) : xs) blocks
      = case testWpShortcode tag of
          Just ("mathjax", _) -> goInlines revAcc xs blocks -- ignore
          Just ("toc", _) -> goInlines (RawInline (P.Format "html") "<!--toc-->" : revAcc) xs blocks
          Just ("TeX-logo", _) -> goInlines (Span ("", ["TeX-logo"], []) [Str "TeX"] : revAcc) xs blocks
          Just ("LaTeX-logo", _) -> goInlines (Span ("", ["LaTeX-logo"], []) [Str "LaTeX"] : revAcc) xs blocks
          Just ("XeTeX-logo", _) -> goInlines (Span ("", ["XeTeX-logo"], []) [Str "XeTeX"] : revAcc) xs blocks
          Just ("XeLaTeX-logo", _) -> goInlines (Span ("", ["XeLaTeX-logo"], []) [Str "XeLaTeX"] : revAcc) xs blocks
          Just ("xypic-logo", _) -> goInlines (Span ("", ["xypic-logo"], []) [Str "Xy-pic"] : revAcc) xs blocks
          Just ("caption", attrs) -> case takeWpBlock "caption" xs of
            (content, rest) ->
              let attrs' = case Data.List.lookup "align" attrs of
                            Just "center" -> ("", ["aligncenter"], [])
                            _ -> ("", [], [])
                  (content', caption) = collectCaption content
                  fig = Figure attrs' (Caption Nothing [Plain caption]) [Plain content']
              in Para (reverse revAcc) : fig : goInlines [] rest blocks
          Just ("code", attrs) ->
              let attr = case Data.List.lookup "lang" attrs of
                           Just lang -> ("", [lang], [])
                           Nothing -> ("", [], [])
              in Para (reverse revAcc) : goSourcecode attr [] xs blocks
          Just ("sourcecode", attrs) ->
              let attr = case Data.List.lookup "lang" attrs of
                           Just lang -> ("", [lang], [])
                           Nothing -> ("", [], [])
              in Para (reverse revAcc) : goSourcecode attr [] xs blocks
          _ -> goInlines (raw : revAcc) xs blocks
    goInlines revAcc (x : xs) blocks = goInlines (x : revAcc) xs blocks
    goInlines revAcc [] blocks = Para (reverse revAcc) : processWpShortcode blocks
    goSourcecode attr revAcc (RawInline (P.Format "wordpress") tag : xs) blocks
      | tag == "[/code]" || tag == "[/sourcecode]" = CodeBlock attr (T.concat (reverse revAcc)) : processWpShortcode (Para xs : blocks)
    goSourcecode attr revAcc (Str s : xs) blocks = goSourcecode attr (s : revAcc) xs blocks
    goSourcecode attr revAcc (P.Space : xs) blocks = goSourcecode attr (" " : revAcc) xs blocks
    goSourcecode attr revAcc (LineBreak : xs) blocks = goSourcecode attr ("\n" : revAcc) xs blocks
    goSourcecode attr revAcc xs@(_ : _) blocks = CodeBlock attr (T.concat (reverse revAcc)) : processWpShortcode (Para xs : blocks)
    goSourcecode attr revAcc@[] [] (Para xs : blocks) = goSourcecode attr revAcc xs blocks -- Immediately after [sourcecode]
    goSourcecode attr revAcc [] (Para xs : blocks) = goSourcecode attr ("\n" : revAcc) xs blocks
    goSourcecode attr revAcc [] blocks = CodeBlock attr (T.concat (reverse revAcc)) : processWpShortcode blocks
processWpShortcode (block : blocks) = block : processWpShortcode blocks
processWpShortcode [] = []

stripEmptyPara :: [Block] -> [Block]
stripEmptyPara (Para [] : xs) = stripEmptyPara xs
stripEmptyPara (x : xs) = x : stripEmptyPara xs
stripEmptyPara [] = []

wpHtmlFilter :: Pandoc -> Pandoc
wpHtmlFilter = P.walk stripEmptyPara . P.walk stripClass . P.walk processWpShortcode . P.walk (mergeStr . parseStr) . P.walk (concatMap stripWpComment) . P.walk attachCodeBlockClass . P.walk (concatMap splitBySoftBreakBlock)
