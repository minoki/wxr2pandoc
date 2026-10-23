{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time (LocalTime (..), TimeOfDay (..), ZonedTime (..),
                            fromGregorian, hoursToTimeZone)
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Definition
import           Text.Pandoc.Extensions
import           Text.Pandoc.Options (ReaderOptions (..), def)
import           Text.Pandoc.Readers.HTML (readHtml)

import           WXR2Pandoc.Filter (hoistMathPreamble, mathPreambleDefinitions,
                                    wpHtmlFilter)
import           WXR2Pandoc.ParseHTML (parseWpHtml)
import           WXR2Pandoc.Write (HTMLReader (..), expandOutputPath,
                                   mathDelimitersToEntities, parsePostWith,
                                   relativizeSiteUrls, renderCommonMark,
                                   separateCodeBlockFromMore,
                                   siteEmbedsToShortcode, siteRelativeUrl,
                                   unflankedEmphasisToHtml, urlRewriterContent)
import           WXR2Pandoc.WXR (Post (..), PostType (..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "WXR2Pandoc"
  [ testGroup "parseWpHtml"
    [ basicTests
    , inlineTests
    , blockTests
    , listTests
    , tableTests
    , mathTests
    , shortcodeTests
    ]
  , testGroup "readHtml + wpHtmlFilter"
    [ pandocBasicTests
    , pandocInlineTests
    , pandocBlockTests
    , pandocFilterTests
    , pandocShortcodeTests
    ]
  , hoistMathPreambleTests
  , mathDelimitersToEntitiesTests
  , unflankedEmphasisToHtmlTests
  , separateCodeBlockFromMoreTests
  , postMetadataTests
  , relativizeSiteUrlsTests
  , siteEmbedsToShortcodeTests
  , urlRewriterContentTests
  ]

-- | Helper to create Pandoc with blocks
doc :: [Block] -> Pandoc
doc = Pandoc mempty

-- | Parse HTML using Pandoc's readHtml and apply wpHtmlFilter
parsePandoc :: T.Text -> Pandoc
parsePandoc html =
  let readerOptions = def
        { readerExtensions = readerExtensions def
            <> extensionsFromList
                 [ Ext_hard_line_breaks
                 , Ext_native_divs
                 , Ext_raw_html
                 , Ext_raw_tex
                 , Ext_tex_math_single_backslash
                 ]
        , readerStripComments = False
        }
  in case runPure (readHtml readerOptions html) of
       Left err  -> error $ show err
       Right ast -> wpHtmlFilter ast

-- | Basic parsing tests
basicTests :: TestTree
basicTests = testGroup "Basic"
  [ testCase "empty input" $
      parseWpHtml "" @?= doc []

  , testCase "plain text becomes paragraph" $
      parseWpHtml "Hello world" @?= doc [Para [Str "Hello", Space, Str "world"]]

  , testCase "simple paragraph" $
      parseWpHtml "<p>Hello</p>" @?= doc [Para [Str "Hello"]]

  , testCase "paragraph with spaces" $
      parseWpHtml "<p>Hello world</p>" @?= doc [Para [Str "Hello", Space, Str "world"]]

  , testCase "multiple paragraphs" $
      parseWpHtml "<p>First</p><p>Second</p>" @?=
        doc [Para [Str "First"], Para [Str "Second"]]

  , testCase "double newline creates paragraph break" $
      parseWpHtml "First\n\nSecond" @?=
        doc [Para [Str "First"], Para [Str "Second"]]

  , testCase "HTML comment is preserved as RawInline" $
      parseWpHtml "<p>Hello <!-- comment --> world</p>" @?=
        doc [Para [Str "Hello", Space, RawInline (Format "html") "<!-- comment -->", Space, Str "world"]]

  , testCase "HTML comment only" $
      parseWpHtml "<!-- just a comment -->" @?=
        doc [Para [RawInline (Format "html") "<!-- just a comment -->"]]
  ]

-- | Inline element tests
inlineTests :: TestTree
inlineTests = testGroup "Inline elements"
  [ testCase "strong" $
      parseWpHtml "<p><strong>bold</strong></p>" @?=
        doc [Para [Strong [Str "bold"]]]

  , testCase "b tag" $
      parseWpHtml "<p><b>bold</b></p>" @?=
        doc [Para [Strong [Str "bold"]]]

  , testCase "em" $
      parseWpHtml "<p><em>italic</em></p>" @?=
        doc [Para [Emph [Str "italic"]]]

  , testCase "i tag" $
      parseWpHtml "<p><i>italic</i></p>" @?=
        doc [Para [Emph [Str "italic"]]]

  , testCase "code" $
      parseWpHtml "<p><code>x = 1</code></p>" @?=
        doc [Para [Code ("", [], []) "x = 1"]]

  , testCase "link" $
      parseWpHtml "<p><a href=\"https://example.com\">link</a></p>" @?=
        doc [Para [Link ("", [], []) [Str "link"] ("https://example.com", "")]]

  , testCase "link with title" $
      parseWpHtml "<p><a href=\"https://example.com\" title=\"Example\">link</a></p>" @?=
        doc [Para [Link ("", [], []) [Str "link"] ("https://example.com", "Example")]]

  , testCase "nested links to the same URL are collapsed" $
      parseWpHtml "<p><a href=\"https://example.com\"><a href=\"https://example.com\">link</a></a></p>" @?=
        doc [Para [Link ("", [], []) [Str "link"] ("https://example.com", "")]]

  , testCase "image" $
      parseWpHtml "<p><img src=\"img.png\" alt=\"Alt text\"></p>" @?=
        doc [Para [Image ("", [], []) [Str "Alt text"] ("img.png", "")]]

  , testCase "strikeout with del" $
      parseWpHtml "<p><del>deleted</del></p>" @?=
        doc [Para [Strikeout [Str "deleted"]]]

  , testCase "strikeout with s" $
      parseWpHtml "<p><s>deleted</s></p>" @?=
        doc [Para [Strikeout [Str "deleted"]]]

  , testCase "subscript" $
      parseWpHtml "<p>H<sub>2</sub>O</p>" @?=
        doc [Para [Str "H", Subscript [Str "2"], Str "O"]]

  , testCase "superscript" $
      parseWpHtml "<p>x<sup>2</sup></p>" @?=
        doc [Para [Str "x", Superscript [Str "2"]]]

  , testCase "nested inline elements" $
      parseWpHtml "<p><strong><em>bold italic</em></strong></p>" @?=
        doc [Para [Strong [Emph [Str "bold", Space, Str "italic"]]]]

  , testCase "br creates line break" $
      parseWpHtml "<p>line1<br>line2</p>" @?=
        doc [Para [Str "line1", LineBreak, Str "line2"]]

  , testCase "quotes in attribute 1" $
      parseWpHtml "<p><a href=\"https://example.com\" title=\"&quot;ABC&quot;\">ABC</a></p>" @?=
        doc [Para [Link ("", [], []) [Str "ABC"] ("https://example.com", "\"ABC\"")]]

  , testCase "quotes in attribute 2" $
      parseWpHtml "<p><x-abc title=\"&quot;ABC&quot;\">ABC</x-abc></p>" @?=
        doc [Para [RawInline (Format "html") "<x-abc title=\"&quot;ABC&quot;\">", Str "ABC", RawInline (Format "html") "</x-abc>"]]
  ]

-- | Block element tests
blockTests :: TestTree
blockTests = testGroup "Block elements"
  [ testCase "headers h1-h6" $
      parseWpHtml "<h1>H1</h1><h2>H2</h2><h3>H3</h3>" @?=
        doc [ Header 1 ("", [], []) [Str "H1"]
            , Header 2 ("", [], []) [Str "H2"]
            , Header 3 ("", [], []) [Str "H3"]
            ]

  , testCase "header with id and class" $
      parseWpHtml "<h2 id=\"intro\" class=\"section\">Intro</h2>" @?=
        doc [Header 2 ("intro", ["section"], []) [Str "Intro"]]

  , testCase "blockquote" $
      parseWpHtml "<blockquote><p>Quote</p></blockquote>" @?=
        doc [BlockQuote [Para [Str "Quote"]]]

  , testCase "pre/code block" $
      parseWpHtml "<pre><code>code here</code></pre>" @?=
        doc [CodeBlock ("", [], []) "code here"]

  , testCase "pre block preserves newlines" $
      parseWpHtml "<pre>line1\nline2</pre>" @?=
        doc [CodeBlock ("", [], []) "line1\nline2"]

  , testCase "pre block with indentation" $
      parseWpHtml "<pre>def foo():\n    return 1\n    </pre>" @?=
        doc [CodeBlock ("", [], []) "def foo():\n    return 1\n    "]

  , testCase "pre block with newline after open tag" $
      parseWpHtml "<pre>\nline1\nline2\n</pre>" @?=
        doc [CodeBlock ("", [], []) "line1\nline2\n"]

  , testCase "pre/code block with newline after tags" $
      parseWpHtml "<pre><code>\nmain = print 1\n</code></pre>" @?=
        doc [CodeBlock ("", [], []) "main = print 1\n"]

  , testCase "hr" $
      parseWpHtml "<p>before</p><hr><p>after</p>" @?=
        doc [Para [Str "before"], HorizontalRule, Para [Str "after"]]

  , testCase "div" $
      parseWpHtml "<div class=\"note\"><p>Note content</p></div>" @?=
        doc [Div ("", ["note"], []) [Para [Str "Note", Space, Str "content"]]]

  , testCase "figure with caption" $
      parseWpHtml "<figure><img src=\"img.png\" alt=\"\"><figcaption>Caption</figcaption></figure>" @?=
        doc [Figure ("", [], [])
              (Caption Nothing [Plain [Str "Caption"]])
              [Plain [Image ("", [], []) [Str ""] ("img.png", "")]]]

  , testCase "blockquote with newline after tag" $
      parseWpHtml "<blockquote class=\"wp-block-quote\"><!-- wp:paragraph -->\n<p>Hello</p>\n<!-- /wp:paragraph --></blockquote>" @?=
        doc [BlockQuote [Para [Str "Hello"]]]

  , testCase "blockquote with custom class" $
      parseWpHtml "<blockquote class=\"twitter-tweet\"><p>Hello</p></blockquote>" @?=
        doc [RawBlock (Format "html") "<blockquote class=\"twitter-tweet\">", Para [Str "Hello"], RawBlock (Format "html") "</blockquote>"]
  ]

-- | List tests
listTests :: TestTree
listTests = testGroup "Lists"
  [ testCase "unordered list" $
      parseWpHtml "<ul><li>Item 1</li><li>Item 2</li></ul>" @?=
        doc [BulletList [[Plain [Str "Item", Space, Str "1"]], [Plain [Str "Item", Space, Str "2"]]]]

  , testCase "ordered list" $
      parseWpHtml "<ol><li>First</li><li>Second</li></ol>" @?=
        doc [OrderedList (1, DefaultStyle, DefaultDelim)
              [[Plain [Str "First"]], [Plain [Str "Second"]]]]

  , testCase "ordered list with start" $
      parseWpHtml "<ol start=\"5\"><li>Fifth</li></ol>" @?=
        doc [OrderedList (5, DefaultStyle, DefaultDelim)
              [[Plain [Str "Fifth"]]]]

  , testCase "nested list" $
      parseWpHtml "<ul><li>Outer<ul><li>Inner</li></ul></li></ul>" @?=
        doc [BulletList [[Plain [Str "Outer"], BulletList [[Plain [Str "Inner"]]]]]]

  , testCase "definition list" $
      parseWpHtml "<dl><dt>Term</dt><dd>Definition</dd></dl>" @?=
        doc [DefinitionList [([Str "Term"], [[Plain [Str "Definition"]]])]]
  ]

-- | Table tests
tableTests :: TestTree
tableTests = testGroup "Tables"
  [ testCase "simple table" $
      let expected = Table
            ("", [], [])
            (Caption Nothing [])
            [(AlignDefault, ColWidthDefault), (AlignDefault, ColWidthDefault)]
            (TableHead ("", [], []) [])
            [TableBody ("", [], []) (RowHeadColumns 0) []
              [ Row ("", [], [])
                  [ Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "A"]]
                  , Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "B"]]
                  ]
              ]]
            (TableFoot ("", [], []) [])
      in parseWpHtml "<table><tr><td>A</td><td>B</td></tr></table>" @?= doc [expected]

  , testCase "table with thead" $
      let expected = Table
            ("", [], [])
            (Caption Nothing [])
            [(AlignDefault, ColWidthDefault)]
            (TableHead ("", [], [])
              [Row ("", [], [])
                [Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Header"]]]])
            [TableBody ("", [], []) (RowHeadColumns 0) []
              [Row ("", [], [])
                [Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "Data"]]]]]
            (TableFoot ("", [], []) [])
      in parseWpHtml "<table><thead><tr><th>Header</th></tr></thead><tbody><tr><td>Data</td></tr></tbody></table>" @?= doc [expected]
  ]

-- | Math tests
mathTests :: TestTree
mathTests = testGroup "Math"
  [ testCase "no-break space in math becomes ordinary space" $
      parseWpHtml "<p>\\(a&nbsp;+\x00A0\&b\\)</p>" @?=
        doc [Para [Math InlineMath "a + b"]]

  , testCase "inline math" $
      parseWpHtml "<p>The formula \\(x^2\\) is simple</p>" @?=
        doc [Para [Str "The", Space, Str "formula", Space, Math InlineMath "x^2", Space, Str "is", Space, Str "simple"]]

  , testCase "latex shortcode" $
      parseWpHtml "<p>Formula: [latex]F(f\\circ g)=F(f)\\circ F(g)[/latex]</p>" @?=
        doc [Para [Str "Formula:", Space, Math InlineMath "F(f\\circ g)=F(f)\\circ F(g)"]]

  , testCase "display math" $
      parseWpHtml "<p>\\[E = mc^2\\]</p>" @?=
        doc [Para [Math DisplayMath "E = mc^2"]]

  , testCase "LaTeX environment" $
      parseWpHtml "<p>\\begin{align}x = 1\\end{align}</p>" @?=
        doc [Para [RawInline (Format "tex") "\\begin{align}x = 1\\end{align}"]]

  , testCase "LaTeX environment with <br>" $
      parseWpHtml "<p>\\begin{align*}x &amp;= 1,\\\\<br>y &amp;= 2,\\\\<br>z &amp;= 3 \\end{align*}</p>" @?=
        doc [Para [RawInline (Format "tex") "\\begin{align*}x &= 1,\\\\\ny &= 2,\\\\\nz &= 3 \\end{align*}"]]
  ]

-- | WordPress shortcode tests for parseWpHtml
shortcodeTests :: TestTree
shortcodeTests = testGroup "Shortcodes"
  [ testCase "escaped shortcode" $
      parseWpHtml "<p>[[LaTeX-logo]] and [[/caption]]</p>" @?=
        doc [Para [Str "[LaTeX-logo]", Space, Str "and", Space, Str "[/caption]"]]

  , testCase "escaped shortcode with attributes" $
      parseWpHtml "<p>[[code lang=\"c\"]]</p>" @?=
        doc [Para [Str "[code", Space, Str "lang=\"c\"]"]]

  , testCase "escaped shortcode in code" $
      parseWpHtml "<p><code>[[LaTeX-logo]]</code></p>" @?=
        doc [Para [Code ("", [], []) "[LaTeX-logo]"]]

  , testCase "double brackets with unknown name are kept" $
      parseWpHtml "<p><code>concat :: [[a]] -&gt; [a]</code></p>" @?=
        doc [Para [Code ("", [], []) "concat :: [[a]] -> [a]"]]

  , testCase "simple shortcode" $
      parseWpHtml "<p>[gallery]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[gallery]"]]

  , testCase "shortcode with attributes" $
      parseWpHtml "<p>[gallery id=\"123\" size=\"medium\"]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[gallery id=\"123\" size=\"medium\"]"]]

  , testCase "closing shortcode" $
      parseWpHtml "<p>[/gallery]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[/gallery]"]]

  , testCase "shortcode pair" $
      parseWpHtml "<p>[code]x = 1[/code]</p>" @?=
        doc [CodeBlock ("", [], []) "x = 1"]

  -- Note: Unclosed [sourcecode] becomes an empty CodeBlock
  , testCase "shortcode with lang attribute" $
      parseWpHtml "<p>[sourcecode lang=\"python\"]</p>" @?=
        doc [CodeBlock ("", ["python"], []) ""]

  , testCase "code shortcode" $
      parseWpHtml "<p>[code lang=\"haskell\"]main = print 1[/code]</p>" @?=
        doc [CodeBlock ("", ["haskell"], []) "main = print 1"]

  , testCase "sourcecode with multiline content" $
      parseWpHtml "<p>[sourcecode lang=\"python\"]def foo():\n    return 1[/sourcecode]</p>" @?=
        doc [CodeBlock ("", ["python"], []) "def foo():\n    return 1"]

  , testCase "sourcecode with newline after open tag" $
      parseWpHtml "<p>[sourcecode lang=\"haskell\"]\nmain = print 1\n[/sourcecode]</p>" @?=
        doc [CodeBlock ("", ["haskell"], []) "main = print 1\n"]

  , testCase "code shortcode with indented multiline" $
      parseWpHtml "<p>[code lang=\"python\"]\ndef foo():\n    x = 1\n    return x\n[/code]</p>" @?=
        doc [CodeBlock ("", ["python"], []) "def foo():\n    x = 1\n    return x\n"]

  , testCase "caption shortcode" $
      parseWpHtml "<p>[caption align=\"center\"]</p>" @?=
        doc [Figure ("", ["aligncenter"], []) (Caption Nothing [Plain []]) [Plain []]]

  , testCase "shortcode mixed with text" $
      parseWpHtml "<p>Before [gallery] after</p>" @?=
        doc [Para [ Str "Before", Space
                  , RawInline (Format "wordpress") "[gallery]"
                  , Space, Str "after"
                  ]]

  , testCase "multiple shortcodes" $
      parseWpHtml "<p>[gallery][/gallery]</p>" @?=
        doc [Para [ RawInline (Format "wordpress") "[gallery]"
                  , RawInline (Format "wordpress") "[/gallery]"
                  ]]

  -- Note: Inside <pre>, shortcodes are currently stripped by collectTextUntil
  -- which only collects TokText and TokNewlines. This is a known limitation.
  , testCase "text inside pre" $
      parseWpHtml "<pre>code here</pre>" @?=
        doc [CodeBlock ("", [], []) "code here"]

  , testCase "code shortcode with multiline content" $
      parseWpHtml "<p>[code]line1\nline2[/code]</p>" @?=
        doc [CodeBlock ("", [], []) "line1\nline2"]

  , testCase "code shortcode with newline after open tag" $
      parseWpHtml "<p>[code]\nline1\n[/code]</p>" @?=
        doc [CodeBlock ("", [], []) "line1\n"]

  , testCase "shortcode with indented content" $
      parseWpHtml "<p>[code]\n    indented\n[/code]</p>" @?=
        doc [CodeBlock ("", [], []) "    indented\n"]

  , testCase "shortcode with hyphen in name" $
      parseWpHtml "<p>[TeX-logo]</p>" @?=
        doc [Para [Span ("", ["TeX-logo"], []) [Str "TeX"]]]

  , testCase "shortcode with number in name" $
      parseWpHtml "<p>[h2o]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[h2o]"]]

  , testCase "closing shortcode with hyphen" $
      parseWpHtml "<p>[/TeX-logo]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[/TeX-logo]"]]

  , testCase "shortcode pair with hyphen" $
      parseWpHtml "<p>[my-code]content[/my-code]</p>" @?=
        doc [Para [ RawInline (Format "wordpress") "[my-code]"
                  , Str "content"
                  , RawInline (Format "wordpress") "[/my-code]"
                  ]]

  , testCase "shortcode ending with number" $
      parseWpHtml "<p>[version2]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[version2]"]]

  , testCase "shortcode with multiple hyphens" $
      parseWpHtml "<p>[my-custom-shortcode]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[my-custom-shortcode]"]]

  , testCase "code shortcode with blank line" $
      parseWpHtml "<p>[code]\ndef foo():\n    pass\n\ndef bar():\n    pass\n[/code]</p>" @?=
        doc [CodeBlock ("", [], []) "def foo():\n    pass\n\ndef bar():\n    pass\n"]

  , testCase "sourcecode with multiple blank lines" $
      parseWpHtml "<p>[sourcecode lang=\"python\"]\nline1\n\n\nline2\n[/sourcecode]</p>" @?=
        doc [CodeBlock ("", ["python"], []) "line1\n\n\nline2\n"]

  , testCase "shortcode inside <a>" $
      parseWpHtml "<p><a href=\"http://www.example.com/\">Hello [TeX-logo]</a></p>" @?=
        doc [Para [Link ("", [], []) [Str "Hello", Space, Span ("", ["TeX-logo"], []) [Str "TeX"]] ("http://www.example.com/", "")]]
  ]

--------------------------------------------------------------------------------
-- Pandoc readHtml + wpHtmlFilter tests
--------------------------------------------------------------------------------

-- | Basic tests for Pandoc reader
pandocBasicTests :: TestTree
pandocBasicTests = testGroup "Basic"
  [ testCase "simple paragraph" $
      parsePandoc "<p>Hello</p>" @?= doc [Para [Str "Hello"]]

  , testCase "paragraph with spaces" $
      parsePandoc "<p>Hello world</p>" @?= doc [Para [Str "Hello",Space,Str "world"]]

  , testCase "multiple paragraphs" $
      parsePandoc "<p>First</p><p>Second</p>" @?=
        doc [Para [Str "First"], Para [Str "Second"]]
  ]

-- | Inline element tests for Pandoc reader
pandocInlineTests :: TestTree
pandocInlineTests = testGroup "Inline elements"
  [ testCase "strong" $
      parsePandoc "<p><strong>bold</strong></p>" @?=
        doc [Para [Strong [Str "bold"]]]

  , testCase "em" $
      parsePandoc "<p><em>italic</em></p>" @?=
        doc [Para [Emph [Str "italic"]]]

  , testCase "code" $
      parsePandoc "<p><code>x = 1</code></p>" @?=
        doc [Para [Code ("", [], []) "x = 1"]]

  , testCase "link" $
      parsePandoc "<p><a href=\"https://example.com\">link</a></p>" @?=
        doc [Para [Link ("", [], []) [Str "link"] ("https://example.com", "")]]

  , testCase "nested inline elements" $
      parsePandoc "<p><strong><em>bold italic</em></strong></p>" @?=
        doc [Para [Strong [Emph [Str "bold",Space,Str "italic"]]]]

  , testCase "quotes in attribute 1" $
      parsePandoc "<p><a href=\"https://example.com\" title=\"&quot;ABC&quot;\">ABC</a></p>" @?=
        doc [Para [Link ("", [], []) [Str "ABC"] ("https://example.com", "\"ABC\"")]]

  , testCase "quotes in attribute 2" $
      parsePandoc "<p><x-abc title=\"&quot;ABC&quot;\">ABC</x-abc></p>" @?=
        doc [Para [RawInline (Format "html") "<x-abc title=\"&quot;ABC&quot;\">", Str "ABC", RawInline (Format "html") "</x-abc>"]]
  ]

-- | Block element tests for Pandoc reader
pandocBlockTests :: TestTree
pandocBlockTests = testGroup "Block elements"
  [ testCase "header" $
      parsePandoc "<h2>Title</h2>" @?=
        doc [Header 2 ("", [], []) [Str "Title"]]

  , testCase "blockquote" $
      parsePandoc "<blockquote><p>Quote</p></blockquote>" @?=
        doc [BlockQuote [Para [Str "Quote"]]]

  , testCase "pre/code block" $
      parsePandoc "<pre><code>code here</code></pre>" @?=
        doc [CodeBlock ("", [], []) "code here"]

  , testCase "unordered list" $
      parsePandoc "<ul><li>Item 1</li><li>Item 2</li></ul>" @?=
        doc [BulletList [[Plain [Str "Item",Space,Str "1"]], [Plain [Str "Item",Space,Str "2"]]]]

  , testCase "ordered list" $
      parsePandoc "<ol><li>First</li><li>Second</li></ol>" @?=
        doc [OrderedList (1, DefaultStyle, DefaultDelim)
              [[Plain [Str "First"]], [Plain [Str "Second"]]]]

  , testCase "definition list" $
      parsePandoc "<dl><dt>Term</dt><dd>Definition</dd></dl>" @?=
        doc [DefinitionList [([Str "Term"], [[Plain [Str "Definition"]]])]]

  , testCase "blockquote with newline after tag" $
      parsePandoc "<blockquote class=\"wp-block-quote\"><!-- wp:paragraph -->\n<p>Hello</p>\n<!-- /wp:paragraph --></blockquote>" @?=
        doc [BlockQuote [Para [Str "Hello"]]]

  , testCase "blockquote with custom class" $
      parsePandoc "<blockquote class=\"twitter-tweet\"><p>Hello</p></blockquote>" @?=
        doc [BlockQuote [Para [Str "Hello"]]]
  ]

-- | wpHtmlFilter specific tests
pandocFilterTests :: TestTree
pandocFilterTests = testGroup "wpHtmlFilter"
  [ testCase "no-break space in math becomes ordinary space" $
      parsePandoc "<p>\\(a&nbsp;+\x00A0\&b\\)</p>" @?=
        doc [Para [Math InlineMath "a + b"]]
  , testCase "strips wp block comments" $
      parsePandoc "<p><!-- wp:paragraph -->Hello<!-- /wp:paragraph --></p>" @?=
        doc [Para [Str "Hello"]]

  , testCase "strips wp-block-code class from CodeBlock" $
      parsePandoc "<pre class=\"wp-block-code\"><code>code</code></pre>" @?=
        doc [CodeBlock ("", [], []) "code"]

  , testCase "strips wp-block-preformatted class" $
      parsePandoc "<pre class=\"wp-block-preformatted\">text</pre>" @?=
        doc [CodeBlock ("", [], []) "text"]

  , testCase "attaches language from syntaxhighlighter comment" $
      parsePandoc "<!-- wp:syntaxhighlighter/code {\"language\":\"haskell\"} --><pre class=\"wp-block-syntaxhighlighter-code\"><code>main = print 1</code></pre><!-- /wp:syntaxhighlighter/code -->" @?=
        doc [CodeBlock ("", ["haskell"], []) "main = print 1"]

  , testCase "parses LaTeX environment" $
      parsePandoc "<p>\\begin{align}x = 1\\end{align}</p>" @?=
        doc [Para [RawInline (Format "tex") "\\begin{align}x = 1\\end{align}"]]

  , testCase "LaTeX environment with <br>" $
      parsePandoc "<p>\\begin{align*}x &amp;= 1,\\\\<br>y &amp;= 2,\\\\<br>z &amp;= 3 \\end{align*}</p>" @?=
        doc [Para [RawInline (Format "tex") "\\begin{align*}x &= 1,\\\\\ny &= 2,\\\\\nz &= 3 \\end{align*}"]]
  ]

-- | WordPress shortcode tests
pandocShortcodeTests :: TestTree
pandocShortcodeTests = testGroup "Shortcodes"
  [ testCase "escaped shortcode" $
      parsePandoc "<p>[[LaTeX-logo]] and [[/caption]]</p>" @?=
        doc [Para [Str "[LaTeX-logo]", Space, Str "and", Space, Str "[/caption]"]]

  , testCase "escaped shortcode with attributes" $
      parsePandoc "<p>[[code lang=\"c\"]]</p>" @?=
        doc [Para [Str "[code lang=\"c\"]"]]

  , testCase "escaped shortcode in code" $
      parsePandoc "<p><code>[[LaTeX-logo]]</code></p>" @?=
        doc [Para [Code ("", [], []) "[LaTeX-logo]"]]

  , testCase "double brackets with unknown name are kept" $
      parsePandoc "<p><code>concat :: [[a]] -&gt; [a]</code></p>" @?=
        doc [Para [Code ("", [], []) "concat :: [[a]] -> [a]"]]

  , testCase "mathjax shortcode is removed" $
      parsePandoc "<p>[mathjax]Hello</p>" @?=
        doc [Para [Str "Hello"]]

  , testCase "latex shortcode becomes inline math" $
      parsePandoc "<p>a [latex](a+b)^n=\\sum_{k=0}^n a_k[/latex] b</p>" @?=
        doc [Para [Str "a", Space, Math InlineMath "(a+b)^n=\\sum_{k=0}^n a_k", Space, Str "b"]]
  , testCase "toc shortcode becomes html comment" $
      parsePandoc "<p>[toc]</p>" @?=
        doc [Para [RawInline (Format "html") "<!--toc-->"]]

  , testCase "sourcecode shortcode with language" $
      parsePandoc "<p>[sourcecode lang=\"python\"]print(1)[/sourcecode]</p>" @?=
        doc [CodeBlock ("", ["python"], []) "print(1)"]

  , testCase "code shortcode" $
      parsePandoc "<p>[code lang=\"haskell\"]main = print 1[/code]</p>" @?=
        doc [CodeBlock ("", ["haskell"], []) "main = print 1"]

  -- Note: Pandoc's Ext_hard_line_breaks converts newlines to LineBreak,
  -- and wpHtmlFilter's goSourcecode collects Str/Space/LineBreak as text.
  -- Leading newlines after shortcode are consumed, and indentation is lost
  -- because LineBreak becomes "\n" but leading spaces are separate tokens.
  , testCase "sourcecode with multiline content" $
      parsePandoc "<p>[sourcecode lang=\"python\"]def foo():\n    return 1[/sourcecode]</p>" @?=
        doc [CodeBlock ("", ["python"], []) "def foo():\nreturn 1"]

  , testCase "sourcecode with newline after open tag" $
      parsePandoc "<p>[sourcecode lang=\"haskell\"]\nmain = print 1\n[/sourcecode]</p>" @?=
        doc [CodeBlock ("", ["haskell"], []) "main = print 1\n"]

  , testCase "code shortcode with indented multiline" $
      parsePandoc "<p>[code lang=\"python\"]\ndef foo():\n    x = 1\n    return x\n[/code]</p>" @?=
        doc [CodeBlock ("", ["python"], []) "def foo():\nx = 1\nreturn x\n"]

  , testCase "caption shortcode creates figure" $
      parsePandoc "<p>[caption align=\"center\"]<img src=\"img.png\"> My caption[/caption]</p>" @?=
        doc [Figure ("", ["aligncenter"], [])
              (Caption Nothing [Plain [Str "My",Space,Str "caption"]])
              [Plain [Image ("", [], []) [] ("img.png", "")]]]

  , testCase "shortcode with hyphen in name" $
      parsePandoc "<p>[TeX-logo]</p>" @?=
        doc [Para [Span ("", ["TeX-logo"], []) [Str "TeX"]]]

  , testCase "shortcode with number in name" $
      parsePandoc "<p>[h2o]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[h2o]"]]

  , testCase "closing shortcode with hyphen" $
      parsePandoc "<p>[/TeX-logo]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[/TeX-logo]"]]

  , testCase "shortcode pair with hyphen" $
      parsePandoc "<p>[my-code]content[/my-code]</p>" @?=
        doc [Para [ RawInline (Format "wordpress") "[my-code]"
                  , Str "content"
                  , RawInline (Format "wordpress") "[/my-code]"
                  ]]

  , testCase "shortcode ending with number" $
      parsePandoc "<p>[version2]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[version2]"]]

  , testCase "shortcode with multiple hyphens" $
      parsePandoc "<p>[my-custom-shortcode]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[my-custom-shortcode]"]]

  , testCase "shortcode inside <a>" $
      parsePandoc "<p><a href=\"http://www.example.com/\">Hello [TeX-logo]</a></p>" @?=
        doc [Para [Link ("", [], []) [Str "Hello", Space, Span ("", ["TeX-logo"], []) [Str "TeX"]] ("http://www.example.com/", "")]]
  ]

-- | Pandoc with the mathPreamble metadata
docWithPreamble :: T.Text -> [Block] -> Pandoc
docWithPreamble preamble = Pandoc (Meta (Map.singleton "mathPreamble" (MetaString preamble)))

hoistMathPreambleTests :: TestTree
hoistMathPreambleTests = testGroup "hoistMathPreamble"
  [ testCase "definitions only" $
      mathPreambleDefinitions "\\def\\Real{\\mathbf{R}} \\newcommand{\\abs}[1]{\\left|#1\\right|}\\renewcommand\\Re{\\operatorname{Re}}\\newcommand{\\f}[2][x]{#1^{#2}}\\def\\pair#1#2{(#1,#2)}" @?=
        Just [ "\\def\\Real{\\mathbf{R}}"
             , "\\newcommand{\\abs}[1]{\\left|#1\\right|}"
             , "\\renewcommand\\Re{\\operatorname{Re}}"
             , "\\newcommand{\\f}[2][x]{#1^{#2}}"
             , "\\def\\pair#1#2{(#1,#2)}"
             ]
  , testCase "\\DeclareMathOperator" $
      mathPreambleDefinitions "\\DeclareMathOperator{\\rank}{rank} \\DeclareMathOperator*{\\argmax}{arg\\,max}\\DeclareMathOperator\\Hom{Hom}" @?=
        Just [ "\\DeclareMathOperator{\\rank}{rank}"
             , "\\DeclareMathOperator*{\\argmax}{arg\\,max}"
             , "\\DeclareMathOperator\\Hom{Hom}"
             ]
  , testCase "\\require is dropped" $
      mathPreambleDefinitions "\\require{AMScd}\\def\\Real{\\mathbf{R}}" @?= Just ["\\def\\Real{\\mathbf{R}}"]
  , testCase "nested braces and escaped braces" $
      mathPreambleDefinitions "\\newcommand{\\set}[1]{\\{#1\\}}\\newcommand{\\t}[1]{{#1}^T}" @?=
        Just ["\\newcommand{\\set}[1]{\\{#1\\}}", "\\newcommand{\\t}[1]{{#1}^T}"]
  , testCase "definition followed by other math" $
      mathPreambleDefinitions "\\def\\Complex{\\mathbf{C}}\\Complex" @?= Nothing
  , testCase "\\require only" $
      mathPreambleDefinitions "\\require{AMScd}" @?= Nothing
  , testCase "ordinary math" $
      mathPreambleDefinitions "x^2" @?= Nothing
  , testCase "hidden span is removed (Pandoc reader)" $
      hoistMathPreamble (parsePandoc "<p><span style=\"display: none;\">\\(\\def\\Real{\\mathbf{R}}\\)</span>Hello \\(\\Real\\)</p>") @?=
        docWithPreamble "\\def\\Real{\\mathbf{R}}\n" [Para [Str "Hello", Space, Math InlineMath "\\Real"]]
  , testCase "hidden span is removed (custom parser)" $
      hoistMathPreamble (parseWpHtml "<p><span style=\"display: none;\">\\(\\def\\Real{\\mathbf{R}}\\)</span>Hello \\(\\Real\\)</p>") @?=
        docWithPreamble "\\def\\Real{\\mathbf{R}}\n" [Para [Str "Hello", Space, Math InlineMath "\\Real"]]
  , testCase "multiple definitions are concatenated" $
      hoistMathPreamble (parsePandoc "<p>\\(\\def\\A{a}\\)A</p><p>\\(\\def\\B{b}\\)</p><p>B</p>") @?=
        docWithPreamble "\\def\\A{a}\n\\def\\B{b}\n" [Para [Str "A"], Para [Str "B"]]
  , testCase "no definitions" $
      hoistMathPreamble (parsePandoc "<p>\\(x\\)</p>") @?= doc [Para [Math InlineMath "x"]]
  ]

mathDelimitersToEntitiesTests :: TestTree
mathDelimitersToEntitiesTests = testGroup "mathDelimitersToEntities"
  [ testCase "brackets in Str" $
      mathDelimitersToEntities [Str "a[b]]c", Space, Str "[d"] @?=
        [ Str "a", RawInline (Format "html") "&lbrack;", Str "b"
        , RawInline (Format "html") "&rbrack;", RawInline (Format "html") "&rbrack;", Str "c"
        , Space, RawInline (Format "html") "&lbrack;", Str "d" ]
  , testCase "dollar signs in Str" $
      mathDelimitersToEntities [Str "$5", Space, Str "${PATH}"] @?=
        [ RawInline (Format "html") "&dollar;", Str "5", Space
        , RawInline (Format "html") "&dollar;", Str "{PATH}" ]
  , testCase "code and math are untouched" $
      mathDelimitersToEntities [Code nullAttr "[$a]", Math InlineMath "[a]"] @?= [Code nullAttr "[$a]", Math InlineMath "[a]"]
  ]

unflankedEmphasisToHtmlTests :: TestTree
unflankedEmphasisToHtmlTests = testGroup "unflankedEmphasisToHtml"
  [ testCase "closing delimiter after punctuation followed by a letter" $
      unflankedEmphasisToHtml [Strong [Str "設計する者は、"], Str "安易に"] @?=
        [RawInline html "<strong>", Str "設計する者は、", RawInline html "</strong>", Str "安易に"]
  , testCase "opening delimiter before punctuation preceded by a letter" $
      unflankedEmphasisToHtml [Str "これは", Emph [Str "「強調」"]] @?=
        [Str "これは", RawInline html "<em>", Str "「強調」", RawInline html "</em>"]
  , testCase "punctuation outside is fine" $
      unflankedEmphasisToHtml [Str "置は「", Strong [Str "あ07"], Str "」です"] @?=
        [Str "置は「", Strong [Str "あ07"], Str "」です"]
  , testCase "punctuation on both sides is fine" $
      unflankedEmphasisToHtml [Str "のか、", Strong [Str "バグが目立つ。"]] @?=
        [Str "のか、", Strong [Str "バグが目立つ。"]]
  , testCase "letters on both sides are fine" $
      unflankedEmphasisToHtml [Str "日本", Strong [Str "語"], Str "です"] @?=
        [Str "日本", Strong [Str "語"], Str "です"]
  , testCase "adjacent emphasis" $
      unflankedEmphasisToHtml [Strong [Str "unboxed", Space, Str "array"], Strong [Str "は相性が悪い"]] @?=
        [RawInline html "<strong>", Str "unboxed", Space, Str "array", RawInline html "</strong>", Strong [Str "は相性が悪い"]]
  , testCase "renders as HTML tags" $
      either (Left . show) Right (renderCommonMark False (doc [Para [Strong [Str "「強調」"], Str "です"]])) @?=
        Right "<strong>「強調」</strong>です\n"
  ]
  where
    html = Format "html"

separateCodeBlockFromMoreTests :: TestTree
separateCodeBlockFromMoreTests = testGroup "separateCodeBlockFromMore"
  [ testCase "divider at the end of a paragraph" $
      separateCodeBlockFromMore [Para [Str "a", more], CodeBlock nullAttr "x"] @?=
        [Para [Str "a", more], RawBlock html "<!-- -->", CodeBlock nullAttr "x"]
  , testCase "divider as a block" $
      separateCodeBlockFromMore [RawBlock html "<!--more-->", CodeBlock nullAttr "x"] @?=
        [RawBlock html "<!--more-->", RawBlock html "<!-- -->", CodeBlock nullAttr "x"]
  , testCase "fenced code block is untouched" $
      separateCodeBlockFromMore [Para [Str "a", more], CodeBlock ("", ["c"], []) "x"] @?=
        [Para [Str "a", more], CodeBlock ("", ["c"], []) "x"]
  , testCase "divider not at the end is untouched" $
      separateCodeBlockFromMore [Para [more, Str "a"], CodeBlock nullAttr "x"] @?=
        [Para [more, Str "a"], CodeBlock nullAttr "x"]
  , testCase "renders the comment" $
      either (Left . show) Right (renderCommonMark False (doc [Para [Str "a", more], CodeBlock nullAttr "#include <x>"])) @?=
        Right "a<!--more-->\n\n<!-- -->\n\n    #include <x>\n"
  ]
  where
    html = Format "html"
    more = RawInline html "<!--more-->"

-- | A post dated 2026-08-01 00:30 +09:00 (2026-07-31 in UTC)
samplePost :: Post
samplePost = Post
  { postType = TypePost
  , postTitle = "Title"
  , postLink = "https://blog.example.com/2026/08/hello/"
  , postPubDate = Nothing
  , postCreator = ""
  , postContent = "<p>Hello</p>"
  , postId = 42
  , postDate = Just (ZonedTime (LocalTime (fromGregorian 2026 8 1) (TimeOfDay 0 30 0)) (hoursToTimeZone 9))
  , postModifiedDate = Nothing
  , postName = "hello"
  , postStatus = "publish"
  , postCategories = []
  , postTags = []
  , postThumbnail = Nothing
  }

postMetadataTests :: TestTree
postMetadataTests = testGroup "parsePostWith metadata"
  [ testCase "years and months in local time" $
      case parsePostWith HTMLReaderCustom Nothing samplePost of
        Left e -> assertFailure (show e)
        Right (Pandoc (Meta meta) _) -> do
          Map.lookup "years" meta @?= Just (MetaList [MetaString "2026"])
          Map.lookup "months" meta @?= Just (MetaList [MetaString "2026/08"])
  , testCase "years and months are written as quoted strings" $
      either (Left . show) Right (parsePostWith HTMLReaderCustom Nothing samplePost >>= renderCommonMark False) @?=
        Right (T.unlines
          [ "---"
          , "date: \"2026-08-01T00:30:00+09:00\""
          , "slug: hello"
          , "title: Title"
          , "wp_id: 42"
          , "years: [\"2026\"]"
          , "months: [\"2026/08\"]"
          , "---"
          , ""
          , "Hello"
          ])
  , testCase "featured image is written as images" $
      either (Left . show) Right (parsePostWith HTMLReaderCustom Nothing samplePost { postThumbnail = Just "https://blog.example.com/wp-content/uploads/2026/08/cover_1.jpg" } >>= renderCommonMark False) @?=
        Right (T.unlines
          [ "---"
          , "date: \"2026-08-01T00:30:00+09:00\""
          , "slug: hello"
          , "title: Title"
          , "wp_id: 42"
          , "years: [\"2026\"]"
          , "months: [\"2026/08\"]"
          , "images: [\"https://blog.example.com/wp-content/uploads/2026/08/cover_1.jpg\"]"
          , "---"
          , ""
          , "Hello"
          ])
  , testCase "output path uses the slug" $
      expandOutputPath "{year}/{monthnum}/{slug}/index.{ext}" Nothing samplePost "md" @?= "2026/08/hello/index.md"
  , testCase "output path uses the post ID for an empty slug" $
      expandOutputPath "{year}/{monthnum}/{slug}/index.{ext}" Nothing samplePost { postName = "" } "md" @?= "2026/08/42/index.md"
  , testCase "no years and months without date" $
      case parsePostWith HTMLReaderCustom Nothing samplePost { postDate = Nothing } of
        Left e -> assertFailure (show e)
        Right (Pandoc (Meta meta) _) -> do
          Map.lookup "years" meta @?= Nothing
          Map.lookup "months" meta @?= Nothing
  , testCase "no years and months for pages" $
      case parsePostWith HTMLReaderCustom Nothing samplePost { postType = TypePage } of
        Left e -> assertFailure (show e)
        Right (Pandoc (Meta meta) _) -> do
          Map.lookup "years" meta @?= Nothing
          Map.lookup "months" meta @?= Nothing
  ]

relativizeSiteUrlsTests :: TestTree
relativizeSiteUrlsTests = testGroup "relativizeSiteUrls"
  [ testCase "page URL with query and fragment" $
      siteRelativeUrl base "https://blog.example.com/2026/07/hello/?a=1#sec" @?= Just ("/2026/07/hello/?a=1#sec", False)
  , testCase "http is the same site" $
      siteRelativeUrl base "http://blog.example.com/2026/07/hello/" @?= Just ("/2026/07/hello/", False)
  , testCase "top page" $
      siteRelativeUrl base "https://blog.example.com" @?= Just ("/", False)
  , testCase "media" $
      siteRelativeUrl base "https://blog.example.com/wp-content/uploads/a.jpg" @?= Just ("/wp-content/uploads/a.jpg", True)
  , testCase "other hosts" $ do
      siteRelativeUrl base "https://example.com/2026/07/hello/" @?= Nothing
      siteRelativeUrl base "https://blog.example.com:8080/2026/07/hello/" @?= Nothing
  , testCase "outside the base path" $
      siteRelativeUrl "https://example.com/blog/" "https://example.com/other/" @?= Nothing
  , testCase "links, images and raw HTML" $
      relativizeSiteUrls False base (doc [Para
        [ Link nullAttr [RawInline (Format "html") "<img src=\"https://blog.example.com/wp-content/uploads/a.jpg\" />"] ("https://blog.example.com/wp-content/uploads/a.jpg", "")
        , Link nullAttr [Str "post"] ("https://blog.example.com/2026/07/hello/", "")
        , Link nullAttr [Str "other"] ("https://example.com/", "")
        , RawInline (Format "html") "<a href=\"https://blog.example.com/2026/07/hello/\">"
        ]]) @?=
      doc [Para
        [ Link nullAttr [RawInline (Format "html") "<img src=\"https://blog.example.com/wp-content/uploads/a.jpg\" />"] ("https://blog.example.com/wp-content/uploads/a.jpg", "")
        , Link nullAttr [Str "post"] ("/2026/07/hello/", "")
        , Link nullAttr [Str "other"] ("https://example.com/", "")
        , RawInline (Format "html") "<a href=\"/2026/07/hello/\">"
        ]]
  , testCase "media and images metadata with includeMedia" $
      relativizeSiteUrls True base (Pandoc (Meta (Map.singleton "images" (MetaList [MetaString "https://blog.example.com/wp-content/uploads/a.jpg"])))
        [Para [Image nullAttr [] ("https://blog.example.com/wp-content/uploads/a.jpg", ""), RawInline (Format "html") "<img src=\"https://blog.example.com/wp-content/uploads/a.jpg\" />"]]) @?=
      Pandoc (Meta (Map.singleton "images" (MetaList [MetaString "/wp-content/uploads/a.jpg"])))
        [Para [Image nullAttr [] ("/wp-content/uploads/a.jpg", ""), RawInline (Format "html") "<img src=\"/wp-content/uploads/a.jpg\" />"]]
  ]
  where
    base = "https://blog.example.com/"

siteEmbedsToShortcodeTests :: TestTree
siteEmbedsToShortcodeTests = testGroup "siteEmbedsToShortcode"
  [ testCase "embed of a page of the site" $
      siteEmbedsToShortcode "blogcard" base (doc [embed "https://blog.example.com/2026/07/hello/"]) @?=
        doc [RawBlock (Format "commonmark") "{{< blogcard \"/2026/07/hello/\" >}}"]
  , testCase "embeds of other sites and media are kept" $ do
      siteEmbedsToShortcode "blogcard" base (doc [embed "https://www.youtube.com/watch?v=x"]) @?= doc [embed "https://www.youtube.com/watch?v=x"]
      siteEmbedsToShortcode "blogcard" base (doc [embed "https://blog.example.com/wp-content/uploads/a.pdf"]) @?= doc [embed "https://blog.example.com/wp-content/uploads/a.pdf"]
  , testCase "rendered as is" $
      either (Left . show) Right (renderCommonMark False (siteEmbedsToShortcode "blogcard" base (doc [embed "https://blog.example.com/2026/07/hello/"]))) @?=
        Right "{{< blogcard \"/2026/07/hello/\" >}}\n"
  ]
  where
    base = "https://blog.example.com/"
    embed url = Figure ("", ["wp-block-embed", "is-type-wp-embed"], []) (Caption Nothing [])
      [Div ("", ["wp-block-embed__wrapper"], []) [Plain [SoftBreak, Str url, SoftBreak]]]

urlRewriterContentTests :: TestTree
urlRewriterContentTests = testGroup "urlRewriterContent"
  [ testCase "published posts and pages" $
      urlRewriterContent
        [ samplePost
        , samplePost { postType = TypePage, postId = 7, postLink = "https://blog.example.com/about/" }
        ] @?= T.unlines
        [ "# Generated by wxr2pandoc generate-url-rewriter"
        , "<IfModule mod_rewrite.c>"
        , "RewriteEngine On"
        , "RewriteCond %{QUERY_STRING} (?:^|&)(?:p|page_id)=42(?:&|$)"
        , "RewriteRule ^(?:index\\.php)?$ /2026/08/hello/ [R=301,L,NE,QSD]"
        , "RewriteCond %{QUERY_STRING} (?:^|&)(?:p|page_id)=7(?:&|$)"
        , "RewriteRule ^(?:index\\.php)?$ /about/ [R=301,L,NE,QSD]"
        , "</IfModule>"
        ]
  , testCase "drafts and query permalinks are skipped" $
      urlRewriterContent
        [ samplePost { postStatus = "draft" }
        , samplePost { postLink = "https://blog.example.com/?p=42" }
        ] @?= T.unlines
        [ "# Generated by wxr2pandoc generate-url-rewriter"
        , "<IfModule mod_rewrite.c>"
        , "RewriteEngine On"
        , "</IfModule>"
        ]
  ]
