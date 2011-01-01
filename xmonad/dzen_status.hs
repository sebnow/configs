module Main where
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Control.Monad.Trans
import Data.Maybe
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Locale (defaultTimeLocale)
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Network.MPD as MPD

type Widget = MVar String -> IO ()

clock :: Widget
clock v = forever $ do
    t <- T.getZonedTime
    putMVar v $ T.formatTime defaultTimeLocale "%a %H:%M" t
    threadDelay (60 * 1000000) -- wait a minute...

-- Small hack to give trayer some room. This should probably be solved
-- elsewhere.
padding :: Widget
padding v = forever $ putMVar v (replicate 6 ' ')

mpd :: Widget
mpd v = forever . MPD.withMPD $ do
    song <- MPD.currentSong
    liftIO $ putMVar v $ maybe "Not playing" formatSong song
    liftIO $ threadDelay 1000000 -- TODO: Wait for event from MPD

formatSong :: MPD.Song -> String
formatSong s = tag MPD.Artist ++ " - " ++ tag MPD.Title
    where tag m = maybe "Unknown" head (M.lookup m (sgTags s))

allWidgets :: [Widget]
allWidgets = [ mpd
             , clock
             , padding
             ]

printStatus :: [MVar String] -> IO ()
printStatus vs = forever $ do
    putStrLn . unwords . map pad =<< mapM takeMVar vs
    threadDelay 1000000

-- Executes a widget returning an MVar which will contain the widget's
-- output.
runWidget :: Widget -> IO (MVar String)
runWidget w = do
    v <- newEmptyMVar
    _ <- forkIO $ w v
    return v

pad :: String -> String
pad s = " " ++ s ++ " "

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    printStatus =<< mapM runWidget allWidgets

