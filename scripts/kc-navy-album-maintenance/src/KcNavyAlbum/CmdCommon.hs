module KcNavyAlbum.CmdCommon
  ( CmdCommon (..)
  , mkCmdCommon
  ) where

import Control.Once
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Kantour.Core.GameResource.Magic
import Kantour.Core.KcData.Master.Direct.Root
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.Exit
import Text.Printf

data CmdCommon = CmdCommon
  { getManager :: IO Manager
  , getMasterRoot :: IO Root
  , doesResourceExist :: String -> IO Bool
  }

loadRawMasterDataFromInternet :: IO BS.ByteString
loadRawMasterDataFromInternet = do
  getManager <- once (newManager tlsManagerSettings)
  mgr <- getManager
  req <- parseRequest "https://raw.githubusercontent.com/Tibowl/api_start2/master/start2.json"
  resp <- httpLbs req mgr
  pure $ BSL.toStrict (responseBody resp)

loadRawMasterData :: IO BS.ByteString
loadRawMasterData =
  maybe loadRawMasterDataFromInternet BS.readFile localSource
  where
    useLocalFile = False
    localSource =
      if useLocalFile
        then Just "/tmp/master.json"
        else Nothing

mkCmdCommon :: IO CmdCommon
mkCmdCommon = do
  raw <- loadRawMasterData
  getManager <- once (newManager tlsManagerSettings)
  getMasterRoot <- once $ do
    case Aeson.eitherDecode @Root (BSL.fromStrict raw) of
      Left msg -> die ("parse error: " <> msg)
      Right r -> pure r

  let doesResourceExist path = do
        mgr <- getManager
        let url = printf "http://%s%s" defaultServer path
        req <- parseRequest url
        resp <- httpNoBody req mgr
        pure $ statusCode (responseStatus resp) == 200

  pure $ CmdCommon {getManager, getMasterRoot, doesResourceExist}
