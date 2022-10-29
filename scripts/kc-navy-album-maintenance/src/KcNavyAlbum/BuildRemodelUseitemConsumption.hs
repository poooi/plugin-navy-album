{-# LANGUAGE DuplicateRecordFields #-}

module KcNavyAlbum.BuildRemodelUseitemConsumption (
  subCmdMain,
) where

{-
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
  in our own codebase.

  Let's assume there's one program given by REMODEL_COST_CALCULATOR,
  which, when given input from stdin, calculates all those info for us
  and gives as stdout the result.
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
  , blueprintCost :: Int
  , steelCost :: Int
  }
  deriving (Show, Generic)

data RemodelInfoResult = RemodelInfoResult
  { mstIdBefore :: Int
  , instantBuildCost :: Int
  , devMatCost :: Int
  }
  deriving (Show, Generic)

instance ToJSON RemodelInfoPrepare

instance ToJSON RemodelInfoResult

instance FromJSON RemodelInfoResult

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain CmdCommon {getMasterRoot} _cmdHelpPrefix = do
  Root {mstShip, mstShipupgrade} <- getMasterRoot
  let upgrades =
        IM.fromList $
          fmap
            (\u@Shipupgrade {currentShipId} -> (currentShipId, u))
            mstShipupgrade
      ships =
        IM.fromList $
          fmap
            (\s@Ship {kcId = shipId} -> (shipId, s))
            mstShip
      prepared :: [RemodelInfoPrepare]
      prepared = do
        Ship
          { kcId = mstIdBefore
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
  let guessDevMat steel bpCost
        | bpCost > 0 || steel < 4500 = 0
        | steel < 5500 = 10
        | steel < 6500 = 15
        | otherwise = 20
      isGuessable
        RemodelInfoResult
          { mstIdBefore
          , instantBuildCost
          , devMatCost
          } = fromMaybe False $ do
          guard $ instantBuildCost == 0
          Ship {afterfuel = Just steelCost} <- ships IM.!? mstIdBefore
          let bpCost = fromMaybe 0 $ do
                Shipupgrade {drawingCount = v} <- upgrades IM.!? mstIdBefore
                pure v
          guard $ devMatCost == guessDevMat steelCost bpCost
          pure True
      (results1, guessables) = partition (not . isGuessable) results0
      results2 = sortOn (\RemodelInfoResult {mstIdBefore = v} -> v) results1
  {-
    To keep asset size small, items that are "guessable" are ignored,
    a "guessable" item meets all of the following criteria:

    - instantBuildCost is 0
    - devMatCost can be derived from steel cost and blueprint cost
      (see `guessDevMat` for that this means)
   -}
  putStrLn $
    "Dropped " <> show (length guessables) <> " items from the result (follow default rules)."
  encodeFile "assets/remodel-info-useitem.json" results2
  putStrLn $ show (length results2) <> " records written."
