module Main where

import System.IO
import qualified Network.Socket as N

-- Configuration options
myServer = "irc.freenode.org" :: String
myPort   = 6667 :: N.PortNumber
myChan   = "#kiwiirc" :: String
myNick   = "lopdan" :: String

-- Connect to a server given its name and port number
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

-- Write to socket
write :: Handle -> String -> String -> IO ()
write h cmd args = do
    let msg = cmd ++ " " ++ args ++ "\r\n"
    hPutStr h msg
    putStr ("> " ++ msg)

--  Listen to socket
listen :: Handle -> IO ()
listen h = forever $ do
    line <- hGetLine h
    putStrLn line
  where
    forever :: IO () -> IO ()
    forever a = do a; forever a

-- Main function
main :: IO ()
main = do
    h <- connectTo myServer myPort
    t <- hGetContents h
    hSetBuffering stdout NoBuffering
    print t