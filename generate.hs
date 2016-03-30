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
import Control.Monad

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

  rss <- read <$> readFile "feed.feed"
  let latestPost = head $ getRssItems rss
      latestPostTitle = head [x | Title x <- latestPost]
      latestPostLink  = head [x | Link x <- latestPost]
      latestPostGuid  = head [x | Guid _ x <- latestPost]

  let generateRss = do
        putStrLn "generating RSS feed"
        writeFile "feed.xml" (showXML $ rssToXML rss)

  let pandoc input output args =
        callProcess "pandoc" $
          args ++
          ["-f", "markdown", "--filter", shortcutLinks] ++
          ["-o", output, input]
  let pandocHtml input output args =
        pandoc input output (args ++ ["-t", "html5", "--standalone"])

  let generateCv = do
        pandocHtml "cv.md" "cv-plain.html" []
        pandoc "cv.md" "cv.pdf" [
          "-t", "latex",
          "-V", "links-as-notes",
          "-V", "geometry:margin=1.5in" ]

  let generatePost f = do
        let outf = dropExtension f
        printf "  * %s\n" outf
        pandocHtml f outf $ concat [
          ["--mathjax"],
          ["--template=page.template", "--css", "/css.css"],
          ["-V", "src:" ++ f],
          ["-V", "url:" ++ outf],
          ["-V", "today:" ++ today],
          if outf `elem` ["index.html", "cv", latestPostGuid]
            then []
            else ["-V", "latest-post-title:" ++ latestPostTitle,
                  "-V", "latest-post-link:" ++ show latestPostLink],
          if outf `elem` ["index.html", "cv"]
            then []
            else ["-V", "comments-enabled"]]

  args <- getArgs
  case args of
    [f] -> generatePost f
    [] -> do
      generateRss
      generateCv
      -- We need doesFileExist because getDirectoryContents can return
      -- symbolic links (like “.#post.md”) that Emacs creates for some reason
      -- and that can't be read
      posts <- filter ((== ".md") . takeExtension) <$>
                 (filterM doesFileExist =<< getDirectoryContents ".")
      mapM_ generatePost posts

instance Read URI where
  readsPrec p s = mapMaybe one $ readsPrec p s
    where one (r, rest) | Just uri <- parseURI r = Just (uri, rest)
                        | otherwise              = Nothing

deriving instance Read CloudProtocol
deriving instance Read ChannelElem
deriving instance Read ItemElem
deriving instance Read RSS
