{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

import BasePrelude
import System.Directory
import System.FilePath
import Text.Printf
import Data.Time
import Data.Foldable
import Text.RSS as RSS
import Network.URI
import Text.Read
import System.Environment
-- import Text.Mustache.Plus
import Data.Aeson
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BS8
import Text.Pandoc as Pandoc
import Text.Pandoc.PDF as Pandoc
import Text.Pandoc.Walk as Pandoc
import Text.Pandoc.Filters.ShortcutLinks


getRssItems :: RSS -> [Item]
getRssItems (RSS _ _ _ _ items) = items

main = do
  home <- getHomeDirectory

  rss <- read <$> readFile "feed.feed"

  let generateRss = do
        putStrLn "generating RSS feed"
        writeFile "output/feed.xml" (showXML $ rssToXML rss)

  let generateCV = do
        renderPage CV "cv.md" "output/cv.html" "cv"
        renderCV_Plain "cv.md" "output/cv-plain.html"
        renderCV_PDF "cv.md" "output/cv.pdf"

  let generateIndex = do
        renderPage Index "index.md" "output/index.html" "index.html"

  -- Post ID is something like "aeson" or "foo/bar"
  let generatePost postId = do
        printf "  * posts/%s\n" postId
        renderPage Post
          ("posts" </> postId <.> "md") ("output" </> postId <.> "html") postId
{-
  let generateMusic ts f = do
        let ident = takeBaseName f
        let outf = dropExtension f
        printf "  * %s\n" outf
        -- when (takeExtension f == ".mustache") $ do
        --   let (rendered, warnings) =
        --         renderMustache mempty
        --         ts{templateActual = fromString ident} Null
        --   mapM_ putStrLn warnings
        --   TL.writeFile (outf <.> "md") rendered
        pandocHtml (outf <.> "md") outf $ concat [
          ["--template=page.template",
           "--css", "/css.css",
           "--css", "/music/css.css",
           "--css", "/jouele/jouele.css",
           "--css", "/jouele/jouele-skin.css" ],
          ["-H", "jouele/jouele.script"],
          ["-V", "src:" ++ f],
          ["-V", "url:" ++ outf],
          ["-V", "today:" ++ today] ]
        -- when (takeExtension f == ".mustache") $ do
        --   removeFile (outf <.> "md")
-}
  args <- getArgs
  case args of
    ["index"] -> generateIndex
    [f] -> generatePost f
    [] -> do
      generateRss
      generateIndex
      generateCV
      -- We need doesFileExist because getDirectoryContents can return
      -- symbolic links (like “.#post.md”) that Emacs creates for some reason
      -- and that can't be read
      posts <-
        fmap (map dropExtension) .
        filterM (\f -> doesFileExist ("posts" </> f)) =<<
        getDirectoryContents "posts"
      mapM_ generatePost posts
      -- do ts <- compileMustacheDir "" "music/"
      --    ps <- compileMustacheDir "" "music/partials/"
      --    music <- filter ((`elem` [".md", ".mustache"]) . takeExtension) <$>
      --               (filterM doesFileExist . map ("music" </>) =<<
      --                getDirectoryContents "music")
      --    mapM_ (generateMusic (ts <> ps)) music

  putStrLn "Done generating!"

instance Read URI where
  readsPrec p s = mapMaybe one $ readsPrec p s
    where one (r, rest) | Just uri <- parseURI r = Just (uri, rest)
                        | otherwise              = Nothing

deriving instance Read CloudProtocol
deriving instance Read ChannelElem
deriving instance Read ItemElem
deriving instance Read RSS

----------------------------------------------------------------------------
-- Page rendering
----------------------------------------------------------------------------

data PageType = Post | Index | CV

renderPage
  :: PageType
  -> FilePath          -- ^ Input file (has to be relative because it's also a Github link)
  -> FilePath          -- ^ Output file
  -> String            -- ^ Page URL (without slash prefix)
  -> IO ()
renderPage pageType input output url = do
  template <- readFile "page.template"
  today <- formatTime defaultTimeLocale "%B %-d, %Y" <$> getCurrentTime
  isoToday <- formatTime defaultTimeLocale "%F" <$> getCurrentTime
  let vars = concat
        [ [("src", input)]
        , [("url", url)]
        , [("today", today)]
        , [("css", "/css.css?date=" ++ isoToday)]
        , case pageType of
            Post ->
              [("comments-enabled", "true")]
            _ -> []
        ]
  mdToHTML
    id
    (\o -> o { writerHTMLMathMethod = KaTeX katexJS katexCSS
             , writerTemplate = template
             , writerVariables = vars
             })
    input
    output
  where
    katexCSS = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0/katex.min.css"
    katexJS  = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.9.0/katex.min.js"

renderCV_Plain :: FilePath -> FilePath -> IO ()
renderCV_Plain = mdToHTML id id

renderCV_PDF :: FilePath -> FilePath -> IO ()
renderCV_PDF =
  mdToPDF
    id
    (\o -> o { writerVariables =
                 [ ("links-as-notes", "true")
                 , ("geometry", "margin=0.8in") ] })

----------------------------------------------------------------------------
-- Pandoc utils
----------------------------------------------------------------------------

-- | Read Markdown.
readMD
  :: (ReaderOptions -> ReaderOptions)
  -> FilePath
  -> IO Pandoc
readMD fReader input = do
  let readerOpts = fReader def
  walkM shortcutLinks
    =<< either throw pure . readMarkdown readerOpts
    =<< readFile input

-- | Convert Markdown to HTML.
mdToHTML
  :: (ReaderOptions -> ReaderOptions)
  -> (WriterOptions -> WriterOptions)
  -> FilePath
  -> FilePath
  -> IO ()
mdToHTML fReader fWriter input output = do
  template <- either throw pure =<< getDefaultTemplate Nothing "html5"
  let writerOpts = fWriter $ def
        { writerHtml5 = True
        , writerStandalone = True
        , writerTemplate = template
        , writerHighlight = True
        }
  writeFile output . writeHtmlString writerOpts
    =<< readMD fReader input

-- | Convert Markdown to a PDF.
mdToPDF
  :: (ReaderOptions -> ReaderOptions)
  -> (WriterOptions -> WriterOptions)
  -> FilePath
  -> FilePath
  -> IO ()
mdToPDF fReader fWriter input output = do
  template <- either throw pure =<< getDefaultTemplate Nothing "latex"
  let writerOpts = fWriter $ def
        { writerStandalone = True
        , writerTemplate = template
        }
  ast <- readMD fReader input
  makePDF "pdflatex" writeLaTeX writerOpts ast >>= \case
    Left err -> error (BS8.unpack err)
    Right bs -> BSL.writeFile output bs
