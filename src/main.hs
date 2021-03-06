import Control.Exception              
import Control.Monad.IO.Class         
import Data.List                      
import System.Exit                    
import System.IO                      
import qualified Network.Socket as N  
import Control.Monad.Trans.Reader     
import System.IO  

-- Configuration options
myServer = "irc.libera.chat" :: String
myPort   = 6667 :: N.PortNumber
myChan   = "#irc-test-bot" :: String
myNick   = "F1Genious" :: String

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . botSocket
    loop st = runReaderT run st

data Bot = Bot { botSocket :: Handle }
type Net = ReaderT Bot IO

-- Bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo myServer myPort
    return (Bot h)
  where
    notify a = bracket_
      (putStrLn ("Connecting to " ++ myServer ++ " ...") >> hFlush stdout)
      (putStrLn "done.")
      a

-- Server connection
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
    addr : _ <- N.getAddrInfo Nothing (Just host) (Just (show port))
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    N.socketToHandle sock ReadWriteMode

-- Process commands from channel
run :: Net ()
run = do
    write "NICK" myNick
    write "USER" (myNick ++ " 0 * :tutorial bot")
    write "JOIN" myChan
    listen

-- Writes in socket
write :: String -> String -> Net ()
write cmd args = do
    h <- asks botSocket
    let msg = cmd ++ " " ++ args ++ "\r\n"
    liftIO $ hPutStr h msg          -- Send message on the wire
    liftIO $ putStr ("> " ++ msg)   -- Show sent message on the command line

-- Listen socket data and process
listen :: Net ()
listen = forever $ do
    h <- asks botSocket
    line <- liftIO $ hGetLine h
    liftIO (putStrLn line)
    let s = init line
    if isPing s then pong s else eval (clean s)
  where
    forever :: Net () -> Net ()
    forever a = do a; forever a

    clean :: String -> String
    clean = drop 1 . dropWhile (/= ':') . drop 1

    isPing :: String -> Bool
    isPing x = "PING :" `isPrefixOf` x

    pong :: String -> Net ()
    pong x = write "PONG" (':' : drop 6 x)

-- Commands
eval :: String -> Net ()
eval "!quit" = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval x | "!wp " `isPrefixOf` x = privmsg (drop 4 x)
eval x | "!tt " `isPrefixOf` x = truthmsg (x)
eval _ = return ()

-- Private message to user
privmsg :: String -> Net ()
privmsg msg = write "PRIVMSG" (myChan ++ " :" ++ msg)

-- Tells the truth in open channel
truthmsg :: String -> Net ()
truthmsg x = readFile "truth.txt" >>= \strFile -> return (write "PRIVMSG" strFile)
