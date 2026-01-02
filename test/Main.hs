{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.Text as T
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Definition
import           Text.Pandoc.Extensions
import           Text.Pandoc.Options (ReaderOptions (..), def)
import           Text.Pandoc.Readers.HTML (readHtml)

import           WXR2Pandoc.Filter (wpHtmlFilter)
import           WXR2Pandoc.ParseHTML (parseWpHtml)

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
              [Para [Image ("", [], []) [Str ""] ("img.png", "")]]]
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
                  [ Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Para [Str "A"]]
                  , Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Para [Str "B"]]
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
                [Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Para [Str "Header"]]]])
            [TableBody ("", [], []) (RowHeadColumns 0) []
              [Row ("", [], [])
                [Cell ("", [], []) AlignDefault (RowSpan 1) (ColSpan 1) [Para [Str "Data"]]]]]
            (TableFoot ("", [], []) [])
      in parseWpHtml "<table><thead><tr><th>Header</th></tr></thead><tbody><tr><td>Data</td></tr></tbody></table>" @?= doc [expected]
  ]

-- | Math tests
mathTests :: TestTree
mathTests = testGroup "Math"
  [ testCase "inline math" $
      parseWpHtml "<p>The formula \\(x^2\\) is simple</p>" @?=
        doc [Para [Str "The", Space, Str "formula", Space, Math InlineMath "x^2", Space, Str "is", Space, Str "simple"]]

  , testCase "display math" $
      parseWpHtml "<p>\\[E = mc^2\\]</p>" @?=
        doc [Para [Math DisplayMath "E = mc^2"]]

  , testCase "LaTeX environment" $
      parseWpHtml "<p>\\begin{align}x = 1\\end{align}</p>" @?=
        doc [Para [RawInline (Format "tex") "\\begin{align}x = 1\\end{align}"]]
  ]

-- | WordPress shortcode tests for parseWpHtml
shortcodeTests :: TestTree
shortcodeTests = testGroup "Shortcodes"
  [ testCase "simple shortcode" $
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
        doc [Para [ RawInline (Format "wordpress") "[code]"
                  , Str "x", Space, Str "=", Space, Str "1"
                  , RawInline (Format "wordpress") "[/code]"
                  ]]

  , testCase "shortcode with lang attribute" $
      parseWpHtml "<p>[sourcecode lang=\"python\"]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[sourcecode lang=\"python\"]"]]

  , testCase "caption shortcode" $
      parseWpHtml "<p>[caption align=\"center\"]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[caption align=\"center\"]"]]

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

  , testCase "shortcode with multiline content" $
      parseWpHtml "<p>[code]line1\nline2[/code]</p>" @?=
        doc [Para [ RawInline (Format "wordpress") "[code]"
                  , Str "line1", SoftBreak, Str "line2"
                  , RawInline (Format "wordpress") "[/code]"
                  ]]

  , testCase "shortcode with newline after open tag" $
      parseWpHtml "<p>[code]\nline1\n[/code]</p>" @?=
        doc [Para [ RawInline (Format "wordpress") "[code]"
                  , SoftBreak, Str "line1", SoftBreak
                  , RawInline (Format "wordpress") "[/code]"
                  ]]

  -- Note: Leading spaces are converted to Space token by textToInlines
  , testCase "shortcode with indented content" $
      parseWpHtml "<p>[code]\n    indented\n[/code]</p>" @?=
        doc [Para [ RawInline (Format "wordpress") "[code]"
                  , SoftBreak, Space, Str "indented", SoftBreak
                  , RawInline (Format "wordpress") "[/code]"
                  ]]

  , testCase "shortcode with hyphen in name" $
      parseWpHtml "<p>[TeX-logo]</p>" @?=
        doc [Para [RawInline (Format "wordpress") "[TeX-logo]"]]

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
  ]

-- | wpHtmlFilter specific tests
pandocFilterTests :: TestTree
pandocFilterTests = testGroup "wpHtmlFilter"
  [ testCase "strips wp block comments" $
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
  ]

-- | WordPress shortcode tests
pandocShortcodeTests :: TestTree
pandocShortcodeTests = testGroup "Shortcodes"
  [ testCase "mathjax shortcode is removed" $
      parsePandoc "<p>[mathjax]Hello</p>" @?=
        doc [Para [Str "Hello"]]

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
  ]
