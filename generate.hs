#!/usr/bin/env runghc

{-# LANGUAGE
StandaloneDeriving,
ScopedTypeVariables
  #-}

-- dependencies: network-uri, rss

import System.Process
import System.Directory
import System.FilePath
import Text.Printf
import Data.Time
import Data.Foldable

import Text.RSS
import Data.Maybe
import Network.URI
import Text.Read
import System.Environment

getRssItems :: RSS -> [Item]
getRssItems (RSS _ _ _ _ items) = items

main = do
  today <- formatTime defaultTimeLocale "%B %-d, %Y" <$> getCurrentTime
  home <- getHomeDirectory
  let shortcutLinks = home </> "code/pandoc-contrib"
                           </> ".cabal-sandbox/bin/pandoc-shortcut-links"

  putStrLn "generating RSS feed"
  rss <- read <$> readFile "feed.feed"
  let latestPost = head $ getRssItems rss
      latestPostTitle = head [x | Title x <- latestPost]
      latestPostLink  = head [x | Link x <- latestPost]
      latestPostGuid  = head [x | Guid _ x <- latestPost]

  writeFile "feed.xml" (showXML $ rssToXML rss)

  putStrLn "converting:"
  posts <- filter ((== ".md") . takeExtension) <$> getDirectoryContents "."
  for_ posts $ \f -> do
    let outf = dropExtension f
    printf "  * %s\n" outf
    callProcess "pandoc" $ concat [
      ["-f", "markdown",
       "-t", "html5",
       "--mathjax",
       "-o", outf,
       "-V", printf "src:%s" f,
       "-V", printf "today:%s" today],
      if outf `elem` ["index.html", "cv", latestPostGuid] then [] else
       ["-V", printf "latest-post-title:%s" latestPostTitle,
        "-V", printf "latest-post-link:%s" (show latestPostLink)],
      ["--template=page.template",
       "--standalone",
       "--css", "/css.css",
       "--filter", shortcutLinks,
       f ]]

  callProcess "pandoc" [
    "-f", "markdown",
    "-t", "latex",
    "-V", "links-as-notes",
    "-V", "geometry:margin=1.5in",
    "-o", "cv.pdf",
    "--filter", shortcutLinks,
    "cv.md" ]

  callProcess "pandoc" [
    "-f", "markdown",
    "-t", "html5",
    "-o", "cv-plain.html",
    "--standalone",
    "--filter", shortcutLinks,
    "cv.md" ]

instance Read URI where
  readsPrec p s = mapMaybe one $ readsPrec p s
    where one (r, rest) | Just uri <- parseURI r = Just (uri, rest)
                        | otherwise              = Nothing

deriving instance Read CloudProtocol
deriving instance Read ChannelElem
deriving instance Read ItemElem
deriving instance Read RSS
