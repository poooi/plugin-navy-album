{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module KcNavyAlbum.BuildRemodelUseitemConsumption
  ( subCmdMain
  )
where

{-
  TODO:

  The useitem here refers to instantBuild and devMat.

  In main.js, this piece of information is obtained by:

  - instantBuild:

    ShipUpgradeModelHolder._getRequiredBuildKitNum(mstIdBefore)

  - devMat:

    ShipUpgradeModelHolder. _getRequiredDevkitNum(mstIdBefore, blueprint, steel)

    where:
    + blueprint is the number of blueprints requires for remodeling
    + steel is the steel consumption for remodeling

  Here the plan is to prepare arguments to those calls and
  have a magic program that takes those input, run the actual function
  and generate a lookup table for us to store as assets.
  This way we avoid having to keep sync-ing with that disgusting piece of code
  in our own codebas (more specifically computeDevMatCount and computeInstantBuildCount).

  Let's assume there's one program given by REMODEL_COST_CALCULATOR,
  which, when given input from stdin, calculates all those info for us
  and gives as stdout the result.

 -}

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as T
import GHC.Generics
import Kantour.Core.KcData.Master.Root
import Kantour.Core.KcData.Master.Ship
import Kantour.Core.KcData.Master.Shipupgrade
import KcNavyAlbum.CmdCommon
import qualified Turtle.Bytes
import Turtle.Prelude

data RemodelInfoPrepare = RemodelInfoPrepare
  { mstIdBefore :: Int
  , blueprintCost :: Int
  , steelCost :: Int
  }
  deriving (Show, Generic)

instance ToJSON RemodelInfoPrepare

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain CmdCommon {getMasterRoot} _cmdHelpPrefix = do
  MasterRoot {mstShip, mstShipupgrade} <- getMasterRoot
  let upgrades =
        M.fromList $
          fmap
            (\u@Shipupgrade {currentShipId} -> (currentShipId, u))
            mstShipupgrade
      prepared :: [RemodelInfoPrepare]
      prepared = do
        Ship
          { shipId = mstIdBefore
          , afterfuel = Just steelCost
          , aftershipid = Just afterShipId
          } <-
          mstShip
        guard $ read (T.unpack afterShipId) > (0 :: Int)
        pure
          RemodelInfoPrepare
            { mstIdBefore
            , steelCost
            , blueprintCost = fromMaybe 0 $ do
                Shipupgrade {drawingCount} <- upgrades M.!? mstIdBefore
                pure drawingCount
            }
  Just remodelCostCalculator <- need "REMODEL_COST_CALCULATOR"
  (ec, outRaw, errRaw) <-
    Turtle.Bytes.procStrictWithErr remodelCostCalculator [] (pure $ BSL.toStrict $ encode prepared)
  print ec
  T.putStrLn (decodeUtf8 outRaw)
  T.putStrLn (decodeUtf8 errRaw)
