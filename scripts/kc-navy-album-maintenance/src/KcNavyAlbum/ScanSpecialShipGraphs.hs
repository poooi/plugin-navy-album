{-# LANGUAGE NamedFieldPuns #-}
module KcNavyAlbum.ScanSpecialShipGraphs
  ( subCmdMain
  ) where

import KcNavyAlbum.CmdCommon
import Kantour.Core.KcData.Master.Root
import Kantour.Core.KcData.Master.Ship
import Control.Monad

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain CmdCommon {getMasterRoot, doesResourceExist} _cmdHelpPrefix = do
  MasterRoot {mstShip} <- getMasterRoot
  let shipIdsToScan = do
        Ship {shipId} <- mstShip
        guard $ shipId <= 1500
        pure shipId
  print shipIdsToScan
