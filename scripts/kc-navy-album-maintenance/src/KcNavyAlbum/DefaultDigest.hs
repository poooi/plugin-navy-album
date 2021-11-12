{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import KcNavyAlbum.CmdCommon

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain CmdCommon {getMasterRoot} _cmdHelpPrefix = do
  parsed <- getMasterRoot
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
