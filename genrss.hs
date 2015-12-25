{-# LANGUAGE
OverloadedStrings,
StandaloneDeriving,
TypeSynonymInstances
  #-}

-- cabal install network-uri rss

import System.FilePath
import Text.RSS
import Data.Maybe
import Network.URI
import Text.Read
import System.Environment

main = mapM_ process =<< getArgs

instance Read URI where
  readsPrec p s = mapMaybe one $ readsPrec p s
    where one (r, rest) | Just uri <- parseURI r = Just (uri, rest)
                        | otherwise              = Nothing

deriving instance Read CloudProtocol
deriving instance Read ChannelElem
deriving instance Read ItemElem
deriving instance Read RSS
  
process :: FilePath -> IO ()
process f = do
  putStrLn $ "generating feed from " ++ f
  desc <- read <$> readFile f
  writeFile (f -<.> "xml") (showXML $ rssToXML desc)
