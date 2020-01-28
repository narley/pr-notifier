{-# LANGUAGE OverloadedStrings #-}

module Bitbucket.Utils where

import           Bitbucket.Types
import qualified Control.Lens      as L
import qualified Network.Wreq      as WR
import qualified RIO.List          as RL
import           RIO.Prelude
import           RIO.Prelude.Types
import qualified RIO.Text          as RT
import qualified Data.ByteString.UTF8 as BU
import Config.Config


createUrlOptions :: [Text] -> String
createUrlOptions =
  RL.intercalate ", "
  . RL.map(
    (\(x:xs) -> '"' : [x] <> xs <> ['"'])
    . RT.unpack
  )

getPullRequests :: String -> String -> IO BBResp
getPullRequests username password = do
  let opts = WR.defaults & WR.auth L.?~ WR.basicAuth (BU.fromString username) (BU.fromString password)
  resp <- WR.asJSON =<< WR.getWith opts baseURL
  return $ resp L.^. WR.responseBody

teamMembersOpenPRs :: Team -> BBResp -> IO PullRequests
teamMembersOpenPRs teamMembers allPRs = do
  return $ filter prsFromTeam $ bbRespPullRequests allPRs
  where
    prsFromTeam prs = elem (teamMember prs) teamMembers && pullRequestIsOpen prs
    teamMember = RT.unpack . authorName . pullRequestAuthor

prsNotApprovedByReviewer :: Text -> PullRequests -> IO [Text]
prsNotApprovedByReviewer reviewer teamOpenPrs = do
  return $
    map (maybe "" repoLink . RL.headMaybe . linksSelf . pullRequestLinks) $
    filter (not . null . filterReviewers . pullRequestReviewers) teamOpenPrs
  where
    reviewerName' = reviewerUserName . reviewerUser
    reviewerApproval = not . reviewerPRApproved
    filterReviewers = filter (\x -> reviewerName' x == reviewer && reviewerApproval x)
