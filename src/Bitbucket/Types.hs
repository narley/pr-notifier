{-# Language NoImplicitPrelude #-}
{-# Language OverloadedStrings #-}

module Bitbucket.Types where

import RIO.Prelude
import RIO.Prelude.Types
import qualified RIO.Text as T
import qualified RIO.List as RL
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE


type PodMembers = [String]

(.->) :: AE.FromJSON a => AE.Parser AE.Object -> Text -> AE.Parser a
(.->) parser key = do
  obj <- parser
  obj AE..: key

baseURL :: String
baseURL = "https://git.impello.co.uk/rest/api/1.0/dashboard/pull-requests"


type PullRequests = [PullRequest]

data BBResp = BBResp
  { bbRespSize :: Integer
  , bbRespPullRequests :: PullRequests
  }
  deriving (Show, Eq)

instance AE.FromJSON BBResp where
  parseJSON (AE.Object o) =
    BBResp <$> o AE..: "size"
           <*> o AE..: "values"
  parseJSON _ = mzero



data Author = Author
  { authorDisplayName :: Text
  , authorName :: Text
  }
  deriving (Show, Eq)

instance AE.FromJSON Author where
  parseJSON = AE.withObject "Author" $ \obj -> do
    displayName <- obj AE..: "user" .-> "displayName"
    name <- obj AE..: "user" .-> "name"
    return $ Author displayName name
  ------------- OR --------------
  -- parseJSON (AE.Object o) =
  --   Author <$> o AE..: "user" .-> "displayName"
  --          <*> o AE..: "user" .-> "name"
  -- parseJSON _ = mzero



data Links = Links
  { linksSelf :: [RepoLink]
  }
  deriving (Show, Eq)

instance AE.FromJSON Links where
  parseJSON (AE.Object o) =
    Links <$> o AE..: "self"
  parseJSON _ = mzero



data RepoLink = RepoLink
  { repoLink :: Text
  }
  deriving (Show, Eq)

instance AE.FromJSON RepoLink where
  parseJSON (AE.Object o) =
    RepoLink <$> o AE..: "href"
  parseJSON _ = mzero



data ReviewerUser = ReviewerUser
  { reviewerUserName :: Text
  , reviewerName :: Text
  }
  deriving (Show, Eq)

instance AE.FromJSON ReviewerUser where
  parseJSON (AE.Object o) =
    ReviewerUser <$> o AE..: "name"
                 <*> o AE..: "displayName"
  parseJSON _ = mzero



data Reviewer = Reviewer
  { reviewerPRApproved :: Bool
  , reviewerUser :: ReviewerUser
  }
  deriving (Show, Eq)

instance AE.FromJSON Reviewer where
  parseJSON (AE.Object o) =
    Reviewer <$> o AE..: "approved"
             <*> o AE..: "user"
  parseJSON _ = mzero



data PullRequest = PullRequest
  { pullRequestTitle :: Text
  , pullRequestLinks :: Links
  , pullRequestAuthor :: Author
  , pullRequestReviewers :: [Reviewer]
  , pullRequestIsOpen :: Bool
  , pullRequestState :: Text
  }
  deriving (Show, Eq)

instance AE.FromJSON PullRequest where
  parseJSON (AE.Object o) =
    PullRequest <$> o AE..: "title"
                <*> o AE..: "links"
                <*> o AE..: "author"
                <*> o AE..: "reviewers"
                <*> o AE..: "open"
                <*> o AE..: "state"
  parseJSON _ = mzero
  ------------- OR --------------
  -- parseJSON = AE.withObject "PullRequest" $ \obj -> do
  --   title <- obj AE..: "title"
  --   author <- obj AE..: "author"
  --   return $ PullRequest title author
