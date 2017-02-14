{-# LANGUAGE OverloadedStrings #-}

module InteroManager (run) where

import Prelude hiding (getLine, putStr, putStrLn)
import System.Process
import System.IO (Handle
                , openFile
                , hSetBuffering
                , IOMode(..)
                , BufferMode(..)
                , hPutChar
                , hSetEncoding
                , utf8)
import Control.Concurrent (forkIO, threadDelay, yield)
import Control.Monad (forever, when)
import Data.Char (digitToInt)
import Data.Text.IO (hGetChunk, hPutStr, hPutStrLn, getLine, hGetLine, putStr, putStrLn)
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Data.IORef

import GhcOutputParser

defaultInteroProcess :: Handle -> CreateProcess
defaultInteroProcess h = CreateProcess {
    cmdspec = ShellCommand "stack exec -- intero",
    cwd = Nothing,
    env = Nothing,
    std_in = CreatePipe,
    std_out = UseHandle h,
    std_err = UseHandle h,
    close_fds = True,
    create_group = False,
    delegate_ctlc = True,
    detach_console = False,
    create_new_console = True,
    new_session = False,
    child_group = Nothing,
    child_user = Nothing
}

run :: IO ()
run = do
  f <- openFile "./intero-manager.log" WriteMode
  hSetBuffering f NoBuffering
  hSetEncoding f utf8

  (sharedRead, sharedWrite) <- createPipe
  hSetBuffering sharedRead NoBuffering
  hSetBuffering sharedWrite NoBuffering
  hSetEncoding sharedWrite utf8
  hSetEncoding sharedRead utf8

  let interoProcess = defaultInteroProcess sharedWrite
  (Just hin, _, _, ph) <- createProcess interoProcess

  buf <- newIORef ("" :: Text)
  forkIO $ forever $ echo sharedRead f buf

  hSetBuffering hin NoBuffering
  setPrompt hin
  forever $ do
    l <- getLine
    hPutStrLn hin l
  return ()

echo :: Handle -> Handle -> IORef Text -> IO ()
echo fromHandle toHandle buf = do
  l <- hGetLine fromHandle
  modifyIORef' buf (<> l <> "\n")
  when (l == prompt) $ do
    -- read contents from the buffer and clear it
    bufContents <- atomicModifyIORef' buf (\cont -> ("", cont))
    -- let parsed = parseErrWarn bufContents
    -- print parsed
    return ()
  hPutStrLn toHandle l
  return ()

setPrompt hin = hPutStrLn hin setPromptCmd

setPromptCmd :: Text
setPromptCmd = ":set prompt \"\\n" <> prompt <> "\\n\""

prompt :: Text
prompt = "XYZPROMPT"
