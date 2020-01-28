{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AppleScript.AppleScript (chooseFromList)
import           Bitbucket.Utils
import           Config.Config
import           Config.Types
import           RIO.List                as RL
import           System.Environment
import           System.Process
import           System.Directory
import qualified Data.Text                as T

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--setup"] -> do
      filePath <- getConfigFilePath
      setup filePath
        >> callCommand "pr-notifier --set-plist"
        >> callCommand "pr-notifier --load-plist"
        >> callCommand "pr-notifier --run"

    ["--set-plist"] -> setPlist -- >> callCommand "pr-notifier --load-plist"

    ["--load-plist"] -> loadPlist -- >> callCommand "pr-notifier --run"

    ["--unload-plist"] -> unloadPlist

    ["--delete-plist"] -> deletePlist

    ["--run"]   -> run
    ["--help"]  -> options
    ["-h"]      -> options
    []          -> options
    _           -> options
  where
    options = putStrLn $
      "===============================\n"
      <> "#         PR-NOTIFIER         #\n"
      <> "===============================\n"
      <> "\n"
      <> "Available options:\n\n"
      <> " --setup:          setup and run this application\n"
      <> " --run:           execute this application\n"
      <> " --set-plist:     create pslist file\n"
      <> " --load-plist:    load(schedule) plist file\n"
      <> " --unload-plist:  unload plist file\n"
      <> " --delete-plist:  delete plist file\n"
      <> " --help:          display this help\n"

setPlist :: IO ()
setPlist = do
  configFilePath <- getConfigFilePath
  configExist <- doesFileExist configFilePath
  plistFilePath <- getPlistFilePath
  plistExist <- doesFileExist plistFilePath
  if configExist then
    do
      config <- loadConfig configFilePath
      if plistExist then do
        unloadPlist
        deletePlist
        createPlist config
      else do
        createPlist config
  else
    putStrLn "Config file not found.\nPlease run 'pr-notifier --setup' again"

run :: IO ()
run = do
  configFilePath <- getConfigFilePath
  (Config username password team reviewer _) <- loadConfig configFilePath
  allPrs <- getPullRequests (T.unpack username) (T.unpack password)
  teamOpenPrs <- teamMembersOpenPRs team allPrs
  prsNotApproved <- prsNotApprovedByReviewer reviewer teamOpenPrs
  if RL.length prsNotApproved > 0 then
    chooseFromList . createUrlOptions $ prsNotApproved
  else
    callCommand ("echo \"Yeah! Nothing to approve! Time to drink some water!\"")
