{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module KcNavyAlbum.ScanSpecialShipGraphs
  ( subCmdMain
  )
where

import Control.Monad
import qualified Data.Text as T
import Kantour.Core.GameResource.Magic
import Kantour.Core.KcData.Master.Root
import Kantour.Core.KcData.Master.Ship
import KcNavyAlbum.CmdCommon
import Text.Printf

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain c@CmdCommon {getMasterRoot} _cmdHelpPrefix = do
  MasterRoot {mstShip} <- getMasterRoot
  let shipsToScan = do
        s@Ship {shipId} <- mstShip
        guard $ shipId <= 1500
        pure s
  forM_ shipsToScan $ \Ship{shipId=mstId, name} -> do
    printf "Scanning %d ...\n" mstId
    xs <- checkSpecialShipGraph c mstId
    forM_ xs $ \graphType ->
      printf "Detected: %s (%d), %s\n" name mstId graphType

checkSpecialShipGraph :: CmdCommon -> Int -> IO [T.Text]
checkSpecialShipGraph CmdCommon {doesResourceExist} mstId = do
  xs <- forM ["special", "special_dmg"] $ \graphType -> do
    let code = magicCode mstId (T.unpack $ "ship_" <> graphType)
    b <- doesResourceExist (printf "/kcs2/resources/ship/%s/%04d_%04d.png" graphType mstId code)
    pure [graphType | b]
  pure $ concat xs
