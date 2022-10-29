module KcNavyAlbum.ScanSpecialShipGraphs (
  subCmdMain,
) where

import Control.Concurrent.Async
import qualified Control.Concurrent.MSem as Sem
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import Kantour.Core.GameResource.Magic
import Kantour.Core.KcData.Master.Direct.Root
import Kantour.Core.KcData.Master.Direct.Ship
import KcNavyAlbum.CmdCommon
import System.IO
import Text.Printf

maxFetchInFlight :: Int
maxFetchInFlight = 10

subCmdMain :: CmdCommon -> String -> IO ()
subCmdMain c@CmdCommon {getMasterRoot} _cmdHelpPrefix = do
  Root {mstShip} <- getMasterRoot
  let shipsToScan = do
        s@Ship {kcId} <- mstShip
        guard $ kcId <= 1500
        pure s

  sem <- Sem.new maxFetchInFlight
  hSetBuffering stdout NoBuffering
  tasks <- forM shipsToScan $ \s@Ship {kcId = mstId} -> async $
    Sem.with sem $ do
      xs <- liftIO $ putStr "." >> checkSpecialShipGraph c mstId
      pure $
        if null xs
          then Nothing
          else Just (s, xs)
  results <- catMaybes <$> mapM wait tasks
  putStrLn ""
  forM_ results $ \(Ship {name}, graphTypes) -> do
    printf "%s: %s\n" name (T.intercalate ", " graphTypes)
  putStrLn "Other than AS:"
  putStrLn "  [ "
  forM_ (filter (\(Ship {stype}, _) -> stype /= 20) results) $ \(Ship {name, kcId = shipId}, gts) -> do
    printf "    // %s: %s\n" name (T.intercalate ", " gts)
    printf "    %d,\n" shipId
  putStrLn "  ].indexOf(mstId) !== -1"

checkSpecialShipGraph :: CmdCommon -> Int -> IO [T.Text]
checkSpecialShipGraph CmdCommon {doesResourceExist} mstId =
  concat
    <$> forM
      ["special", "special_dmg"]
      ( \graphType -> do
          let code = magicCode mstId (T.unpack $ "ship_" <> graphType)
          b <- doesResourceExist (printf "/kcs2/resources/ship/%s/%04d_%04d.png" graphType mstId code)
          pure [graphType | b]
      )
