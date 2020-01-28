{-# LANGUAGE OverloadedStrings #-}

module Config.Config where

import           Config.Types
import           Data.List                (head, last, map)
import           Data.List.Split
import qualified Data.Text                as T
import           RIO
import           RIO.List                 as RL
import           System.Console.Haskeline
import           System.Directory
import           System.Process


baseURL :: String
baseURL = "https://git.impello.co.uk/rest/api/1.0/dashboard/pull-requests"

listPairToTuplePair :: [[String]] -> [(Text, String)]
listPairToTuplePair =
  map (\xs -> (T.pack $ head xs, last xs))

makeListPair :: [String] -> [[String]]
makeListPair = map (splitOn "=")

getConfigFilePath :: IO FilePath
getConfigFilePath = do
  homeDir <- getUserDocumentsDirectory
  return $ homeDir <> "/.pr-notifier"

plistFileName :: String
plistFileName = "uk.co.shellenergy.contra.pr-notifier.plist"

getPlistFilePath :: IO FilePath
getPlistFilePath = do
  homeDir <- getUserDocumentsDirectory
  return $ homeDir <> "/Library/LaunchAgents/" <> plistFileName

trim :: String -> String
trim = T.unpack . T.strip . T.pack

createPlist :: Config -> IO ()
createPlist (Config _ _ _ _ interval)= do
  putStrLn "Creating new pslist file..."
  homeDir <- getUserDocumentsDirectory
  filePath <- getPlistFilePath
  writeFile filePath $ mkPlist homeDir interval
  putStrLn $ filePath <> " was successfully created"
  where
    mkPlist homeDir interval =
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
      <> "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
      <> "<plist version=\"1.0\">\n"
      <> "  <dict>\n"
      <> "    <key>Label</key>\n"
      <> "    <string>" <> plistFileName <> "</string>\n"
      <> "    <key>ProgramArguments</key>\n"
      <> "    <array>\n"
      <> "      <string>" <> homeDir <> "/.local/bin/pr-notifier</string>\n"
      <> "      <string>--run</string>\n"
      <> "    </array>\n"
      <> "    <key>StartInterval</key>\n"
      <> "    <integer>" <> (show interval) <> "</integer>\n"
      <> "  </dict>\n"
      <> "</plist>\n"

loadPlist :: IO ()
loadPlist = do
  configFilePath <- getConfigFilePath
  (Config _ _ _ _ interval) <- loadConfig configFilePath
  plistFilePath <- getPlistFilePath
  callCommand $ "launchctl load -w " <> plistFilePath
  putStrLn "plist file loaded"
  putStrLn "==============================="
  putStrLn $ "All done! You will be notified about your team's PR every "
    <> show interval <> "s"

unloadPlist :: IO ()
unloadPlist = do
  filePath <- getPlistFilePath
  callCommand $ "launchctl unload -w " <> filePath
  putStrLn "pslist file unloaded"

deletePlist :: IO ()
deletePlist = do
  filePath <- getPlistFilePath
  callCommand $ "rm " <> filePath
  putStrLn "pslist file deleted"

setUserConfig :: IO Config
setUserConfig = runInputT defaultSettings $ do
  outputStrLn "Let's config this app, shall we?"
  maybeUsername <- getInputLine "Bitbucket username: "
  maybePassword <- getPassword (Just '*') "Bitbucket password: "
  maybeTeam <- getInputLine "Team (team members username separated by comma): "
  maybeReviewer <- getInputLine "Bitbucket reviewer: "
  maybeInterval <- getInputLine "Notification interval (in seconds): "
  return $
    Config
      (maybe "" T.pack maybeUsername)
      (maybe "" T.pack maybePassword)
      (maybe [] (map trim . splitOn ",") maybeTeam)
      (maybe "" T.pack maybeReviewer)
      (maybe 300 (\x -> fromMaybe 300 (readMaybe x)) maybeInterval)

createFile :: FilePath -> Config -> IO ()
createFile filePath (Config username password team reviewer interval) = do
  putStrLn "Creating config file..."
  writeFile filePath config
  putStrLn $ filePath <> " was successfully created"
  where
    config =
      "USERNAME=" <> T.unpack username <> "\n"
      <> "PASSWORD=" <> T.unpack password <> "\n"
      <> "TEAM=" <> (RL.intercalate [','] team) <> "\n"
      <> "REVIEWER=" <> T.unpack reviewer <> "\n"
      <> "INTERVAL=" <> (show interval) <> "\n"

loadConfig :: FilePath -> IO Config
loadConfig filePath = do
  content <- readFile filePath
  let configPairs = listPairToTuplePair . makeListPair . lines $ content
  let username = maybe "" T.pack $ lookup "USERNAME" configPairs
  let password = maybe "" T.pack $ lookup "PASSWORD" configPairs
  let team = maybe [] (splitOn ",") $ lookup "TEAM" configPairs
  let reviewer = maybe "" T.pack $ lookup "REVIEWER" configPairs
  let interval = case lookup "INTERVAL" configPairs of
        Just x  -> fromMaybe 500 $ readMaybe x
        Nothing -> 500
  return $ Config username password team reviewer interval

setup :: FilePath -> IO ()
setup filePath = do
  putStrLn $
    "===============================\n"
    <> "#                             #\n"
    <> "#         PR-NOTIFIER         #\n"
    <> "#                             #\n"
    <> "===============================\n"
  configExist <- doesFileExist filePath
  if configExist then
    return ()
  else
    do
      config' <- setUserConfig
      createFile filePath config'
