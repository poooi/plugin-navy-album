module KcNavyAlbum.CmdCommon
  ( CmdCommon (..)
  )
where

import Kantour.Core.KcData.Master.Root
import Network.HTTP.Client

data CmdCommon = CmdCommon
  { getManager :: IO Manager
  , getMasterRoot :: IO MasterRoot
  }
