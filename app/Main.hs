{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Main where

import Bitbucket.Utils (prsNotAprovedByDeveloper, createUrlOptions)
import AppleScript.AppleScript (chooseFromList)
import RIO.Prelude
import RIO.Prelude.Types
import RIO.List as RL
import System.Process

main :: IO ()
main = do
  prs <- prsNotAprovedByDeveloper ["nbrittes", "rdean", "abowden", "mrehman"] "nbrittes"
  if (RL.length prs > 0) then
    chooseFromList . createUrlOptions $ prs
  else
    callCommand ("echo \"Yeah! Nothing to approve! Time to drink some water!\"")
