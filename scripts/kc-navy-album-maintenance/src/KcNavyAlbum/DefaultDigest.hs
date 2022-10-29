{-# LANGUAGE DuplicateRecordFields #-}

module KcNavyAlbum.DefaultDigest where

import Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V
import Kantour.Core.KcData.Master.Direct.Root
import Kantour.Core.KcData.Master.Direct.Ship
import Kantour.Core.KcData.Master.Direct.Shipgraph
import Kantour.Core.KcData.Master.Direct.Slotitem
import KcNavyAlbum.CmdCommon

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain CmdCommon {getMasterRoot} _cmdHelpPrefix = do
  parsed <- getMasterRoot
  encodeFile "assets/default-digest.json" (mkDigest parsed)
  putStrLn "Written to default-digest.json."

mkDigest :: Root -> Value
mkDigest Root {mstSlotitem, mstShipgraph, mstShip} =
  Object $
    KM.fromList
      [
        ( "equipMstIds"
        , Array $
            V.fromList $
              fmap (Number . fromIntegral) $
                sort $ fmap Kantour.Core.KcData.Master.Direct.Slotitem.kcId mstSlotitem
        )
      ,
        ( "shipDigests"
        , Array $
            V.fromList $
              fmap
                ( \shipId ->
                    Array $
                      V.fromList
                        [ Number (fromIntegral shipId)
                        , String (graphDigestTable IM.! shipId)
                        ]
                )
                $ sort $ fmap (\Ship {kcId = shipId} -> shipId) mstShip
        )
      ]
  where
    graphDigestTable :: IM.IntMap T.Text
    graphDigestTable = IM.fromList $ fmap mk mstShipgraph
      where
        mk Shipgraph {kcId, filename, version} =
          (kcId, filename <> "#" <> NE.head version)
