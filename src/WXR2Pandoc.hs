{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module WXR2Pandoc where
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

namespace_dc, namespace_wp, namespace_content :: Maybe T.Text
namespace_dc = Just "http://purl.org/dc/elements/1.1/"
namespace_wp = Just "http://wordpress.org/export/1.2/"
namespace_content = Just "http://purl.org/rss/1.0/modules/content/"

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

data Post = Post { postTitle :: T.Text
                 , postLink :: T.Text
                 , postPubDate :: Maybe UTCTime
                 , postCreator :: T.Text
                 , postContent :: T.Text
                 , postId :: Int
                 , postDate :: Maybe LocalTime
                 , postDateGmt :: Maybe LocalTime
                 , postName :: T.Text
                 , postStatus :: T.Text -- publish, draft, inherit
                 -- , postType :: T.Text -- post, attachment
                 , postCategories :: [T.Text]
                 , postTags :: [T.Text]
                 }
            deriving (Eq, Show)

-- 記事データを output/<slug>.txt および output/<slug>-raw.txt に書き出す。
-- 前者は Markdown に変換したデータ、後者は生データ
renderPostToFile :: Post -> IO ()
renderPostToFile post@Post{..} = do
  let name = if T.null postName
             then show postId
             else T.unpack postName
  let path = "output/" ++ name ++ ".md"
  let meta = Meta $ Map.fromList $
               [("title", MetaString postTitle)]
               ++ [("draft", MetaBool True) | postStatus == "draft"]
               ++ [("categories", MetaList (map MetaString postCategories))]
               ++ [("tags", MetaList (map MetaString postTags))]
               ++ [("date", MetaString $ T.pack $ formatTime defaultTimeLocale rfc822DateFormat pd) | Just pd <- [postDate]]
  let readerOptions :: ReaderOptions
      readerOptions = def { readerExtensions = readerExtensions def <> extensionsFromList [Ext_hard_line_breaks, Ext_native_divs, Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash]
                          , readerStripComments = False
                          }
      writerOptions :: WriterOptions
      writerOptions = def { writerExtensions = writerExtensions def <> extensionsFromList [Ext_raw_html, Ext_raw_tex, Ext_tex_math_single_backslash, Ext_yaml_metadata_block]
                          , writerWrapText = P.WrapPreserve
                          }
      pandocAction = do doc <- readHtml readerOptions postContent
                        tplResult <- runWithPartials $ compileTemplate "" "$if(titleblock)$\n$titleblock$\n\n$endif$\n$body$\n"
                        let tpl = case tplResult of
                                    Left e -> error e
                                    Right t -> t
                        let Pandoc _ blocks = wpHtmlFilter doc
                        writeCommonMark (writerOptions { writerTemplate = Just tpl }) $ Pandoc meta blocks
  case runPure pandocAction of
    Left err -> print err
    Right md -> T.writeFile path md
  case runPure (readHtml readerOptions postContent >>= \doc -> writeXML writerOptions (wpHtmlFilter doc)) of
    Left err -> print err
    Right doc -> do
      let path_pd = "output/" ++ name ++ ".xml"
      T.writeFile path_pd doc
  case runPure (readHtml readerOptions postContent >>= \doc -> writeJSON writerOptions (wpHtmlFilter doc)) of
    Left err -> print err
    Right doc -> do
      let path_json = "output/" ++ name ++ ".json"
      T.writeFile path_json doc
  let path_raw = "output/" ++ name ++ "-raw.txt"
  T.writeFile path_raw $ postTitle <> "\n\n" <> postContent

processFile_xmlconduit :: FilePath -> IO ()
processFile_xmlconduit filename = do
  doc <- XC.readFile XC.def filename
  let doc_cursor = XC.fromDocument doc
  let [channel] = doc_cursor XC.$| XC.child >=> XC.element (XC.Name "channel" Nothing Nothing)
      items = channel XC.$| XC.child >=> XC.element (XC.Name "item" Nothing Nothing)
  forM_ items $ \item -> do
    -- element (Name "title" Nothing Nothing) :: Axis
    let post_type = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_type" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let title = T.dropWhileEnd isSpace $ T.dropWhile isSpace $ mconcat (item XC.$| XC.child >=> XC.element (XC.Name "title" Nothing Nothing) >=> XC.child >=> XC.content)
    let link_content = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "link" Nothing Nothing) >=> XC.child >=> XC.content)
    let pubDate = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "pubDate" Nothing Nothing) >=> XC.child >=> XC.content) -- rfc822DateFormat %a, %_d %b %Y %H:%M:%S %Z
        pubDate_p = parseTimeM @Maybe @UTCTime True defaultTimeLocale rfc822DateFormat (T.unpack pubDate)
    let creator = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "creator" namespace_dc Nothing) >=> XC.child >=> XC.content)
    let content_encoded = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "encoded" namespace_content Nothing) >=> XC.child >=> XC.content)
    let post_id = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_id" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let post_date = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_date" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_date_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_date)
    let post_date_gmt = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_date_gmt" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_date_gmt_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_date_gmt)
    let post_modified = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_modified" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_modified_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_modified)
    let post_modified_gmt = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_modified_gmt" namespace_wp Nothing) >=> XC.child >=> XC.content) -- yyyy-mm-dd hh:mm:ss
        post_modified_gmt_p = parseTimeM @Maybe @LocalTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack post_modified_gmt)
    let post_name = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "post_name" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let post_status = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "status" namespace_wp Nothing) >=> XC.child >=> XC.content)
    let isCategory (XC.Element _ attrs _) = case Map.lookup (XC.Name "domain" Nothing Nothing) attrs of
                                              Just "category" -> True
                                              _ -> False
    let isTag (XC.Element _ attrs _) = case Map.lookup (XC.Name "domain" Nothing Nothing) attrs of
                                              Just "post_tag" -> True
                                              _ -> False
    let categories = item XC.$| XC.child >=> XC.element (XC.Name "category" Nothing Nothing) >=> XC.checkElement isCategory >=> XC.child >=> XC.content
    let tags = item XC.$| XC.child >=> XC.element (XC.Name "category" Nothing Nothing) >=> XC.checkElement isTag >=> XC.child >=> XC.content
    -- <wp:comment>...</wp:comment>
    -- print (title, link_content, pubDate_p, creator, post_name, post_id)
    case post_type of
      "post" -> do
        renderPostToFile Post{ postTitle = title
                             , postLink = link_content
                             , postPubDate = pubDate_p
                             , postCreator = creator
                             , postContent = content_encoded
                             , postId = case T.decimal post_id of
                                          Left err -> error err
                                          Right (x, _) -> x
                             , postDate = post_date_p
                             , postDateGmt = post_date_gmt_p
                             , postName = post_name
                             , postStatus = post_status
                             , postCategories = categories
                             , postTags = tags
                             }
      "page" -> putStrLn $ T.unpack post_name ++ " / " ++ T.unpack title
      "attachment" -> do
        let attachment_url = mconcat (item XC.$| XC.child >=> XC.element (XC.Name "attachment_url" namespace_wp Nothing) >=> XC.child >=> XC.content)
        -- putStrLn $ T.unpack attachment_url
        pure ()
      "nav_menu_item" -> pure ()
      "wp_global_styles" -> pure ()
      "wp_navigation" -> pure ()
      "custom_css" -> pure ()
      _ -> putStrLn $ "Unrecognized post type: " ++ T.unpack post_type
