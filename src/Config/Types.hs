module Config.Types where

import           Bitbucket.Types
import qualified RIO.Text        as RT

data Config = Config
  { configUsername :: RT.Text
  , configPassword :: RT.Text
  , configTeam     :: Team
  }
  deriving (Show)
