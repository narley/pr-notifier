{-# LANGUAGE OverloadedStrings #-}

module Config.Config where

import qualified Data.Text as T
import qualified System.IO.Strict as IOS
import System.Console.Haskeline
import System.IO
import System.Directory
import Control.DeepSeq as DS
import Control.Exception


getUserConfig :: IO ()
getUserConfig = runInputT defaultSettings $ do
  outputStrLn "Let's config this app, shall we?"
  username <- getInputLine "Bitbucket username: "
  password <- getPassword (Just '*') "Bitbucket password: "
  podMembers <- getInputLine "Pod member (pod members username separated by space) :"
  outputStrLn "foooo"

checkFile = do
  homeDir <- getUserDocumentsDirectory
  -- exceptionOrContent <- try $ readFile (homeDir <> "/.pr-notifier")
  content <- Control.Exception.catch (readFile (homeDir <> "/.pr-notifier"))
        (\e -> do
            let err = show (e :: IOException)
            hPutStr stderr ("Warning: Couldn't open " ++ homeDir ++ ": " ++ err)
            return ""
        )
  putStrLn content
  -- case exceptionOrContent of
  --   Left except -> print except :: IO ()
  --   -- Right content -> DS.rnf content `seq` putStrLn content
  --   Right content -> putStrLn content
