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
        s <- mstShip
        guard $ s.kcId <= 1500
        pure s

  sem <- Sem.new maxFetchInFlight
  hSetBuffering stdout NoBuffering
  tasks <- forM shipsToScan $ \s -> async $
    Sem.with sem $ do
      xs <- liftIO $ putStr "." >> checkSpecialShipGraph c s.kcId
      pure $
        if null xs
          then Nothing
          else Just (s, xs)
  results <- catMaybes <$> mapM wait tasks
  putStrLn ""
  forM_ results $ \(s, graphTypes) -> do
    printf "%s: %s\n" s.name (T.intercalate ", " graphTypes)
  putStrLn "Other than AS:"
  putStrLn "  [ "
  forM_ (filter (\(s, _) -> s.stype /= 20) results) $ \(s, gts) -> do
    printf "    // %s: %s\n" s.name (T.intercalate ", " gts)
    printf "    %d,\n" s.kcId
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
