module Main where
import Network.HTTP (getRequest, simpleHTTP, getResponseBody)
import Control.Exception (try, IOException)
import Control.Applicative ((<$>))
import Text.Feed.Import
import Text.Feed.Types
import System.IO

urlForFeed :: String
urlForFeed = "http://haskell.mn/atom.xml"

grabFeedText :: String -> IO (Maybe String)
grabFeedText url = do
  rsp <- simpleHTTP (getRequest url)
  result <- try $ getResponseBody rsp :: IO (Either IOException String)
  return $ either (const Nothing) Just result

main :: IO ()
main = do
  result <- flip (>>=) parseFeedString <$> grabFeedText urlForFeed
  case result of
    Nothing  -> hPutStrLn stderr $ "No feed found at" ++ urlForFeed
    Just str -> print str
