{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module KcNavyAlbum.CmdCommon
  ( CmdCommon (..)
  , mkCmdCommon
  )
where

import Control.Once
import qualified Data.Aeson as Aeson
import Kantour.Core.GameResource.Magic
import Kantour.Core.KcData.Master.Root
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.Exit
import Text.Printf

data CmdCommon = CmdCommon
  { getManager :: IO Manager
  , getMasterRoot :: IO MasterRoot
  , doesResourceExist :: String -> IO Bool
  }

mkCmdCommon :: IO CmdCommon
mkCmdCommon = do
  getManager <- once (newManager tlsManagerSettings)
  getMasterRoot <- once $ do
    mgr <- getManager
    req <- parseRequest "https://raw.githubusercontent.com/kcwiki/kancolle-data/master/api/api_start2.json"
    resp <- httpLbs req mgr
    case Aeson.eitherDecode @MasterRoot (responseBody resp) of
      Left msg -> die ("parse error: " <> msg)
      Right r -> pure r

  let doesResourceExist path = do
        mgr <- getManager
        req <- parseRequest (printf "http://%s%s" defaultServer path)
        resp <- httpNoBody req mgr
        pure $ statusCode (responseStatus resp) == 200

  pure $ CmdCommon {getManager, getMasterRoot, doesResourceExist}
