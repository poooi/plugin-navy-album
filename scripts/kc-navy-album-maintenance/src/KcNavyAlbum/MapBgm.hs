{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KcNavyAlbum.MapBgm where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.IntSet as IS
import qualified Data.Text as T
import Kantour.GameResource.Magic
import Network.HTTP.Client
import Network.HTTP.Types
import Text.Printf
import Turtle.Prelude
import Prelude hiding (FilePath)

subCmdMain :: Manager -> String -> IO ()
subCmdMain mgr _cmdHelpPrefix = do
  let goodGap = 16
  knownBgms <-
    Aeson.eitherDecodeFileStrict @IS.IntSet "assets/map-bgms.json" >>= \case
      Left e -> die (T.pack e)
      Right s -> pure s

  let detectBgm curId mLastGoodId
        | case mLastGoodId of
            Nothing -> False
            Just lastGoodId -> curId - lastGoodId > goodGap = do
          pure []
        | IS.member curId knownBgms =
          detectBgm (curId + 1) (Just curId)
        | otherwise = do
          putStr $ "Checking BGM #" <> show curId <> " ..."
          s <- checkBattleBgm mgr curId
          if s == 200
            then do
              putStrLn " yes"
              (curId :) <$> detectBgm (curId + 1) (Just curId)
            else do
              putStrLn " no"
              detectBgm (curId + 1) mLastGoodId
  newBgms <- detectBgm 1 Nothing
  let knownBgms' = IS.union knownBgms (IS.fromList newBgms)
  if knownBgms /= knownBgms'
    then do
      Aeson.encodeFile @[Int] "assets/map-bgms.json" (IS.toAscList knownBgms')
      putStrLn "Map BGM asset updated."
    else putStrLn "Nothing to do."

checkBattleBgm :: Manager -> Int -> IO Int
checkBattleBgm mgr bgmId = do
  let code = magicCode bgmId "bgm_battle"
  req <- parseRequest (printf "http://%s/kcs2/resources/bgm/battle/%03d_%04d.mp3" defaultServer bgmId code)
  resp <- httpNoBody req mgr
  pure (statusCode $ responseStatus resp)
