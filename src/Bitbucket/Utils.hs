{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Bitbucket.Utils where

import           Bitbucket.Types
import qualified Control.Lens      as L
import qualified Network.Wreq      as WR
import qualified RIO.List          as RL
import           RIO.Prelude
import           RIO.Prelude.Types
import qualified RIO.Text          as RT


createUrlOptions :: [Text] -> String
createUrlOptions =
  RL.intercalate ", "
  . RL.map(
    (\(x:xs) -> '"' : [x] <> xs <> ['"'])
    . RT.unpack
  )

getPullRequests :: IO BBResp
getPullRequests = do
  let opts = WR.defaults & WR.auth L.?~ WR.basicAuth "user_name_here" "user_password_here"
  resp <- WR.asJSON =<< WR.getWith opts baseURL
  return $ resp L.^. WR.responseBody

podMembersOpenPRs :: Team -> IO PullRequests
podMembersOpenPRs podMembers = do
  allPRs <- getPullRequests
  return $ filter prsFromTeam $ bbRespPullRequests allPRs
  where
    prsFromTeam prs = elem (podMember prs) podMembers && pullRequestIsOpen prs
    podMember = RT.unpack . authorName . pullRequestAuthor

prsNotAprovedByDeveloper :: [String] -> String -> IO [Text]
prsNotAprovedByDeveloper podMembers developer = do
  prs <- podMembersOpenPRs podMembers
  return $
    map (maybe "" repoLink . RL.headMaybe . linksSelf . pullRequestLinks) $
    filter (not . null . filterReviewers . pullRequestReviewers) prs
  where
    reviewerName' = reviewerUserName . reviewerUser
    reviewerApproval = not . reviewerPRApproved
    filterReviewers = filter (\x -> reviewerName' x == RT.pack developer && reviewerApproval x)
