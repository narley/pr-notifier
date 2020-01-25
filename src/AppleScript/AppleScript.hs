{-# LANGUAGE OverloadedStrings #-}

module AppleScript.AppleScript
    ( chooseFromList
    ) where

import System.Process

chooseFromList :: String -> IO ()
chooseFromList urls =
  callCommand (
  "osascript -e 'choose from list {" <> urls <> "} with title \"PRs I Need To Review\" with prompt \"Select PR to review:\" with multiple selections allowed \n"
  <> "if result is not false then \n"
  <> "    tell application \"Google Chrome\" to tell front window \n"
  <> "        repeat with prUrl in result \n"
  <> "            set newTab to make new tab with properties {URL: prUrl} \n"
  <> "        end repeat \n"
  <> "    end tell \n"
  <> "end if '"
  )
