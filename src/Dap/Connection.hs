module Dap.Connection where

import Dap.Protocol
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Network.Simple.TCP qualified as TCP
import Network.Socket.ByteString (recv)
import Network.WebSockets.Stream qualified as Stream
import Utils (addContentLength)

data Connection = Connection
  { read :: IO BS.ByteString
  , write :: BS.ByteString -> IO ()
  , close :: IO ()
  }

makeConnection :: IO BS.ByteString -> (BS.ByteString -> IO ()) -> IO () -> IO Connection
makeConnection read write close = do
  pure $! Connection {..}

socketConnection :: TCP.Socket -> IO Connection
socketConnection socket = do
  makeConnection
    (recv socket 8192)
    (TCP.send socket)
    (TCP.closeSock socket)

openSocketConnection :: TCP.HostName -> TCP.ServiceName -> IO Connection
openSocketConnection host port = do
  (socket, remoteAddr) <- TCP.connectSock host port
  -- TCP.connect host port \(socket, remoteAddr) -> do
  putStrLn $ "Connection established to " ++ show remoteAddr ++ " Socket: " <> show socket
  socketConnection socket
