{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AppleScript.AppleScript (chooseFromList)
import           Bitbucket.Utils         (createUrlOptions,
                                          prsNotAprovedByDeveloper)
import           RIO.List                as RL
import           RIO.Prelude
import           RIO.Prelude.Types
import           System.Process

main :: IO ()
main = do
  -- prs <- prsNotAprovedByDeveloper ["user1", "user2", "user3", "user4"] "user1"
  prs <- prsNotAprovedByDeveloper ["nbrittes", "rdean", "mrehman", "abowden"] "nbrittes"
  if RL.length prs > 0 then
    chooseFromList . createUrlOptions $ prs
    -- callCommand ("echo \"Just testing this\"")
  else
    callCommand ("echo \"Yeah! Nothing to approve! Time to drink some water!\"")
