{-# LANGUAGE OverloadedStrings #-}

module Config.Config where

import           Control.DeepSeq          as DS
import           Control.Exception
-- import           Data.HashMap             (fromList, lookup)
import           Config.Types
import           Data.List                (head, last, map)
import           Data.List.Split
import qualified Data.Text                as T
import           RIO
import           System.Console.Haskeline
import           System.Directory
import           System.IO
import qualified System.IO.Strict         as IOS

listPairToTuplePair :: [[String]] -> [(Text, String)]
listPairToTuplePair xss =
  map (\xs -> (T.pack $ head xs, last xs)) xss

makeListPair :: [String] -> [[String]]
makeListPair xs = map (splitOn "=") xs

configFilePath :: IO FilePath
configFilePath = do
  homeDir <- getUserDocumentsDirectory
  return $ homeDir <> "/.pr-notifier"

getUserConfig :: IO ()
getUserConfig = runInputT defaultSettings $ do
  outputStrLn "Let's config this app, shall we?"
  username <- getInputLine "Bitbucket username: "
  password <- getPassword (Just '*') "Bitbucket password: "
  podMembers <- getInputLine "Pod member (pod members username separated by space) :"
  outputStrLn "foooo"

loadConfig :: IO Config
loadConfig = do
  filePath <- configFilePath
  content <- readFile filePath
  let configPairs = listPairToTuplePair . makeListPair . lines $ content
  let username = maybe "" (T.pack) $ lookup ("USERNAME") configPairs
  let password = maybe "" (T.pack) $ lookup ("PASSWORD") configPairs
  let team = maybe [] (splitOn ",") $ lookup ("TEAM") configPairs
  return $ Config username password team

checkFile :: IO String
checkFile = do
  filePath <- configFilePath
  configExist <- doesFileExist filePath
  if configExist then
    do
      config <- loadConfig
      return $ show config
  else
    createFile
