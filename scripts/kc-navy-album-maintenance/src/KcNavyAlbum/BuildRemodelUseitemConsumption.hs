{-# LANGUAGE DuplicateRecordFields #-}

module KcNavyAlbum.BuildRemodelUseitemConsumption
  ( subCmdMain
  ) where

{-
  The useitem here refers to instantBuild, devMat, gunMat and screw
  (and probably more items in the future that can only be extracted from game code).

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
  in our own codebase.

  Let's assume there's one program given by REMODEL_COST_CALCULATOR,
  which, when given input from stdin, calculates all those info for us
  and gives as stdout the result.

  TODO: there is also some extra bit of logic involved in ShipUpgradeModel.

  In particular:

  - newhokohesosizai (or GunMat):
    + requires property `mst_id_after` (aka. plain data `api_id`)
    + requires property `mst_id_before` (aka. plain data `api_current_ship_id`)
  - revkit (or Screw):
    + requires property `mst_id_after` (aka. plain data `api_id`)

  In future we should consider passing down raw master data directly instead of
  current method of manually reconstructing what is being used.

 -}

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IM
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics
import Kantour.Core.KcData.Master.Direct.Root
import Kantour.Core.KcData.Master.Direct.Ship
import Kantour.Core.KcData.Master.Direct.Shipupgrade
import KcNavyAlbum.CmdCommon
import System.Exit
import qualified Turtle.Bytes
import Turtle.Prelude hiding (die, sortOn)

data RemodelInfoPrepare = RemodelInfoPrepare
  { mstIdBefore :: Int
  , mstIdAfter :: Int
  , blueprintCost :: Int
  , steelCost :: Int
  }
  deriving (Show, Generic)

data RemodelInfoResult = RemodelInfoResult
  { mstIdBefore :: Int
  , instantBuildCost :: Int
  , devMatCost :: Int
  , gunMatCost :: Int
  , screwCost :: Int
  }
  deriving (Show, Generic)

instance ToJSON RemodelInfoPrepare

instance ToJSON RemodelInfoResult

instance FromJSON RemodelInfoResult

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain CmdCommon {getMasterRoot} _cmdHelpPrefix = do
  Root {mstShip, mstShipupgrade} <- getMasterRoot
  let
    upgrades =
      IM.fromList $
        fmap
          (\u@Shipupgrade {currentShipId} -> (currentShipId, u))
          mstShipupgrade
    prepared :: [RemodelInfoPrepare]
    prepared = do
      Ship
        { kcId = mstIdBefore
        , afterfuel = Just steelCost
        , aftershipid = Just afterShipId
        } <-
        mstShip
      [(afterId, "")] <- pure $ reads @Int (T.unpack afterShipId)
      guard $ afterId > 0
      pure
        RemodelInfoPrepare
          { mstIdBefore
          , mstIdAfter = afterId
          , steelCost
          , blueprintCost = fromMaybe 0 $ do
              Shipupgrade {drawingCount} <- upgrades IM.!? mstIdBefore
              pure drawingCount
          }
  Just remodelCostCalculator <- need "REMODEL_COST_CALCULATOR"
  (ec, outRaw, errRaw) <-
    Turtle.Bytes.procStrictWithErr remodelCostCalculator [] (pure $ BSL.toStrict $ encode prepared)
  case ec of
    ExitFailure {} -> do
      putStrLn $ "REMODEL_COST_CALCULATOR failed with: " <> show ec
      die (T.unpack $ decodeUtf8 errRaw)
    ExitSuccess -> pure ()
  results0 <- case eitherDecode' @[RemodelInfoResult] (BSL.fromStrict outRaw) of
    Right v -> pure v
    Left errMsg -> do
      die ("JSON parse error: " <> errMsg)
  {-
    TODO: we should probably not guess at all - the reason we want a lookup table is because
    we don't want those spagetti code in our codebase.
   -}
  let
    isGuessable _ = False
    (results1, _) = partition (not . isGuessable) results0
    results2 = sortOn (\RemodelInfoResult {mstIdBefore = v} -> v) results1
  {-
    To keep asset size small, items that are "guessable" are ignored,
    a "guessable" item meets all of the following criteria:

    - instantBuildCost is 0
    - devMatCost can be derived from steel cost and blueprint cost
      (see `guessDevMat` for that this means)
   -}
  encodeFile "assets/remodel-info-useitem.json" results2
  putStrLn $ show (length results2) <> " records written."
