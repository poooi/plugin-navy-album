{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module KcNavyAlbum.Main
  ( main
  )
where

{-
  Required environment variables:

  - NAVY_ALBUM_REPO: path to the repo.

 -}

import Control.Monad
import qualified Data.Set as S
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (null)
import qualified KcNavyAlbum.BuildRemodelUseitemConsumption
import KcNavyAlbum.CmdCommon
import qualified KcNavyAlbum.DefaultDigest
import qualified KcNavyAlbum.MapBgm
import qualified KcNavyAlbum.ScanSpecialShipGraphs
import System.Environment
import System.Exit
import Turtle.Prelude hiding (die)
import Prelude hiding (FilePath)

updateKcReplay :: IO ()
updateKcReplay = do
  (ec, out) <- procStrict "npm" ["run", "update-kcreplay"] ""
  case ec of
    ExitSuccess -> putStrLn "Updated sucessfully."
    ExitFailure x -> do
      putStrLn (T.unpack out)
      die $ "code: " <> show x

main :: IO ()
main = do
  Just fp <- need "NAVY_ALBUM_REPO"
  cd (fromText fp)
  common <- mkCmdCommon
  getArgs >>= \case
    subCmd : args
      | Just handler <- lookup subCmd handlers ->
        withArgs args (handler common ("<prog> " <> subCmd <> " "))
    _ -> do
      forM_ handlers $ \(sub, _) ->
        putStrLn $ "<prog> " <> sub <> " ..."
      exitFailure
  where
    defSubCmds = S.fromList ["map-bgm", "default-digest", "update-kcreplay"]
    runDef m p = forM_ handlers $ \(w, action) ->
      when (S.member w defSubCmds) $ do
        putStrLn $ "Running " <> w <> " ..."
        action m p
    handlers =
      [ ("map-bgm", KcNavyAlbum.MapBgm.subCmdMain)
      , ("default-digest", KcNavyAlbum.DefaultDigest.subCmdMain)
      , ("update-kcreplay", \_ _ -> updateKcReplay)
      , ("scan-special-ship-graphs", KcNavyAlbum.ScanSpecialShipGraphs.subCmdMain)
      , ("build-remodel-useitem-consumption", KcNavyAlbum.BuildRemodelUseitemConsumption.subCmdMain)
      , ("def", runDef)
      ]
