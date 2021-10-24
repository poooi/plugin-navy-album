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
import qualified Data.Text as T
import Filesystem.Path.CurrentOS hiding (null)
import qualified KcNavyAlbum.DefaultDigest
import qualified KcNavyAlbum.MapBgm
import Network.HTTP.Client
import Network.HTTP.Client.TLS
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
  mgr <- newManager tlsManagerSettings

  getArgs >>= \case
    subCmd : args
      | Just handler <- lookup subCmd handlers ->
        withArgs args (handler mgr ("<prog> " <> subCmd <> " "))
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
      , ("all", runAll)
      ]
