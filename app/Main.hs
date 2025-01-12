{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List as List
import Network.Simple.TCP

host = BS.pack "Host"

get = BS.pack "\"\\\"GET"

echo = BS.pack "echo"

main :: IO ()
main = do
  let host = "127.0.0.1"
      port = "4221"

  BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port

  serve (Host host) port $ \(serverSocket, serverAddr) -> do
    BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show serverAddr)

    res <- recv serverSocket 1024

    case res of
      Just e -> do
        print $ show e
        let segment = getLastRouteSegment $ BS.split ' ' e
        print segment

        case segment of
          Just x -> send serverSocket $ BS.pack $ response x
          Nothing -> send serverSocket "HTTP/1.1 200 OK\r\n\r\n"
      Nothing -> send serverSocket "HTTP/1.1 404 Not Found\r\n\r\n"

response :: String -> String
response s =
  "HTTP/1.1 200 OK\r\n\
  \Content-Type: text/plain\r\n\
  \Content-Length: "
    ++ show (length s)
    ++ "\r\n\r\n"
    ++ s
    ++ "\r\n\r\n"

checkRoot :: Maybe String -> Bool
checkRoot str =
  case str of
    Just s -> s == "/"
    Nothing -> False

getPath :: [BS.ByteString] -> Maybe String
getPath [] = Nothing
getPath (host : path : _) = Just $ BS.unpack path
getPath (_ : xs) = getPath xs

getLastRouteSegment :: [BS.ByteString] -> Maybe String
getLastRouteSegment [] = Nothing
getLastRouteSegment (_ : path : _) = do
  case BS.split '/' path of
    (z : x : y : xs) | x == echo -> Just $ BS.unpack y
    _ -> Nothing
