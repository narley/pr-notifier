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


listPairToTuplePair :: [[String]] -> [(Text, String)]
listPairToTuplePair =
  map (\xs -> (T.pack $ head xs, last xs))

makeListPair :: [String] -> [[String]]
makeListPair = map (splitOn "=")

configFilePath :: IO FilePath
configFilePath = do
  homeDir <- getUserDocumentsDirectory
  return $ homeDir <> "/.pr-notifier"

trim :: String -> String
trim = T.unpack . T.strip . T.pack


setUserConfig :: IO Config
setUserConfig = runInputT defaultSettings $ do
  outputStrLn "Let's config this app, shall we?"
  maybeUsername <- getInputLine "Bitbucket username: "
  maybePassword <- getPassword (Just '*') "Bitbucket password: "
  maybeTeam <- getInputLine "Team (team members username separated by comma): "
  maybeInterval <- getInputLine "Notification interval (in seconds): "
  return $
    Config
      (maybe "" T.pack maybeUsername)
      (maybe "" T.pack maybePassword)
      (maybe [] (map trim . splitOn ",") maybeTeam)
      (maybe 300 (\x -> fromMaybe 300 (readMaybe x)) maybeInterval)


createFile :: Config -> IO ()
createFile (Config username password team interval) = do
  let config =
        "USERNAME=" <> T.unpack username <> "\n"
        <> "PASSWORD=" <> T.unpack password <> "\n"
        <> "TEAM=" <> (RL.intercalate [','] team) <> "\n"
        <> "INTERVAL=" <> (show interval) <> "\n"
  filePath <- configFilePath
  writeFile filePath config



loadConfig :: IO Config
loadConfig = do
  filePath <- configFilePath
  content <- readFile filePath
  let configPairs = listPairToTuplePair . makeListPair . lines $ content
  let username = maybe "" T.pack $ lookup "USERNAME" configPairs
  let password = maybe "" T.pack $ lookup "PASSWORD" configPairs
  let team = maybe [] (splitOn ",") $ lookup "TEAM" configPairs
  let interval = case lookup "INTERVAL" configPairs of
        Just x  -> fromMaybe 500 $ readMaybe x
        Nothing -> 500
  return $ Config username password team interval

setUp :: IO ()
setUp = do
  filePath <- configFilePath
  configExist <- doesFileExist filePath
  if configExist then
    return ()
  else
    do
      config' <- setUserConfig
      createFile config'
      putStrLn $ "All done! You will be notified about your team's PR every "
        <> show (configInterval config') <> " seconds."
