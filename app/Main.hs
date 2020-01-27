{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AppleScript.AppleScript (chooseFromList)
import           Bitbucket.Utils         (createUrlOptions,
                                          prsNotAprovedByDeveloper)
import           Config.Config
import           Config.Types
import           RIO.List                as RL
import           RIO.Prelude.Types
import           System.Environment
import           System.Exit
import           System.Process

-- main :: IO ()
-- main = do
--   -- prs <- prsNotAprovedByDeveloper ["user1", "user2", "user3", "user4"] "user1"
--   prs <- prsNotAprovedByDeveloper ["nbrittes", "rdean", "mrehman", "abowden"] "nbrittes"
--   if RL.length prs > 0 then
--     chooseFromList . createUrlOptions $ prs
--     -- callCommand ("echo \"Just testing this\"")
--   else
--     callCommand ("echo \"Yeah! Nothing to approve! Time to drink some water!\"")



-- TESTING 1
-- main = getArgs >>= parse >>= putStr . tac
main = do
  args <- getArgs
  case args of
    ["--setup"] -> setUp >> callCommand "pr-notifier --run"
    ["-s"]      -> setUp >> callCommand "pr-notifier --run"
    ["--run"]   -> putStrLn "--run"
    ["-r"]      -> putStrLn "-r"
    []          -> options
    _           -> options
  where
    options = putStrLn $
      "Available options:\n --setup: setup this application\n"
      <> " --run: execute this application"



-- TODO: Figure out this flow

-- PROGRAM FLOW
-- pr-notifier --run
-- main =
--   if config file exist then
--     load config
--     if plist file exist then
--        delete plist file
--        create and load new plist file
--     else
--        create and load new plist file
--   else
--      prompt user to provide config info
--      if plist file exist then
--         delete plist flie
--         create and load new plist file
--
