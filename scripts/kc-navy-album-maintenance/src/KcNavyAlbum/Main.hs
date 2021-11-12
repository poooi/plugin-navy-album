{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KcNavyAlbum.Main
  ( main
  )
where

{-
  Required environment variables:

  - NAVY_ALBUM_REPO: path to the repo.

 -}

import Control.Monad
import Control.Once
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (null)
import Kantour.Core.KcData.Master.Root
import KcNavyAlbum.CmdCommon
import qualified KcNavyAlbum.DefaultDigest
import qualified KcNavyAlbum.MapBgm
import qualified KcNavyAlbum.ScanSpecialShipGraphs
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import System.Environment
import System.Exit
import Turtle.Prelude hiding (die)
import Prelude hiding (FilePath)
import Text.Printf
import Kantour.Core.GameResource.Magic
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

  common <- do
    getManager <- once (newManager tlsManagerSettings)
    getMasterRoot <- once $ do
      mgr <- getManager
      req <- parseRequest "https://raw.githubusercontent.com/kcwiki/kancolle-data/master/api/api_start2.json"
      resp <- httpLbs req mgr
      case Aeson.eitherDecode @MasterRoot (responseBody resp) of
        Left msg -> die ("parse error: " <> msg)
        Right r -> pure r

    let doesResourceExist path = do
          mgr <- getManager
          req <- parseRequest (printf "http://%s%s" defaultServer path)
          resp <- httpNoBody req mgr
          pure $ statusCode (responseStatus resp) == 200

    pure $ CmdCommon {getManager, getMasterRoot, doesResourceExist}

  getArgs >>= \case
    subCmd : args
      | Just handler <- lookup subCmd handlers ->
        withArgs args (handler common ("<prog> " <> subCmd <> " "))
    _ -> do
      forM_ handlers $ \(sub, _) ->
        putStrLn $ "<prog> " <> sub <> " ..."
      exitFailure
  where
    runAll m p = forM_ handlers $ \(w, action) ->
      unless (w == "all") $ do
        putStrLn $ "Running " <> w <> " ..."
        action m p
    handlers =
      [ ("map-bgm", KcNavyAlbum.MapBgm.subCmdMain)
      , ("default-digest", KcNavyAlbum.DefaultDigest.subCmdMain)
      , ("update-kcreplay", \_ _ -> updateKcReplay)
      , ("scan-special-ship-graphs", KcNavyAlbum.ScanSpecialShipGraphs.subCmdMain)
      , ("all", runAll)
      ]
