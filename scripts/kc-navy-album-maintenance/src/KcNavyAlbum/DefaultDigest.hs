{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module KcNavyAlbum.DefaultDigest where

import Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V
import Kantour.Core.KcData.Master.Root
import Kantour.Core.KcData.Master.Ship
import Kantour.Core.KcData.Master.Shipgraph
import Kantour.Core.KcData.Master.Slotitem
import Network.HTTP.Client
import System.Exit

subCmdMain :: Manager -> String -> IO ()
subCmdMain mgr _cmdHelpPrefix = do
  req <- parseRequest "https://raw.githubusercontent.com/kcwiki/kancolle-data/master/api/api_start2.json"
  resp <- httpLbs req mgr
  parsed <- case Aeson.eitherDecode @MasterRoot (responseBody resp) of
    Left msg -> die ("parse error: " <> msg)
    Right r -> pure r
  encodeFile "assets/default-digest.json" (mkDigest parsed)
  putStrLn "Written to default-digest.json."

mkDigest :: MasterRoot -> Value
mkDigest MasterRoot {mstSlotitem, mstShipgraph, mstShip} =
  Object $
    HM.fromList
      [ ( "equipMstIds"
        , Array $
            V.fromList $
              fmap (Number . fromIntegral) $
                sort $ fmap slotId mstSlotitem
        )
      , ( "shipDigests"
        , Array $
            V.fromList $
              fmap
                (\shipId ->
                   Array $
                     V.fromList
                       [ Number (fromIntegral shipId)
                       , String (graphDigestTable IM.! shipId)
                       ])
                $ sort $ fmap (\Ship {shipId} -> shipId) mstShip
        )
      ]
  where
    graphDigestTable :: IM.IntMap T.Text
    graphDigestTable = IM.fromList $ fmap mk mstShipgraph
      where
        mk Shipgraph {shipId, filename, version} =
          (shipId, filename <> "#" <> NE.head version)
