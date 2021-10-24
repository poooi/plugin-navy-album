{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module KcNavyAlbum.DefaultDigest where

import Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V
import Deriving.Aeson
import Network.HTTP.Client
import System.Exit

data KcApiField

instance StringModifier KcApiField where
  getStringModifier = ("api_" <>)

type KcConvention = [CamelToSnake, KcApiField]

data KcMasterData = KcMasterData
  { mstSlotitem :: [KcSlotitem]
  , mstShipgraph :: [KcShipgraph]
  , mstShip :: [KcShip]
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier KcConvention]
          KcMasterData

data KcSlotitem = KcSlotitem
  { slotId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "slotId" "id" : KcConvention)]
          KcSlotitem

data KcShipgraph = KcShipgraph
  { version :: NE.NonEmpty T.Text
  , filename :: T.Text
  , shipId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          KcShipgraph

data KcShip = KcShip
  { shipId :: Int
  }
  deriving stock (Generic, Show)
  deriving
    (FromJSON)
    via CustomJSON
          '[FieldLabelModifier (Rename "shipId" "id" : KcConvention)]
          KcShip

subCmdMain :: Manager -> String -> IO ()
subCmdMain mgr _cmdHelpPrefix = do
  req <- parseRequest "https://raw.githubusercontent.com/kcwiki/kancolle-data/master/api/api_start2.json"
  resp <- httpLbs req mgr
  parsed <- case Aeson.eitherDecode @KcMasterData (responseBody resp) of
    Left msg -> die ("parse error: " <> msg)
    Right r -> pure r
  encodeFile "assets/default-digest.json" (mkDigest parsed)
  putStrLn "Written to default-digest.json."

mkDigest :: KcMasterData -> Value
mkDigest KcMasterData {mstSlotitem, mstShipgraph, mstShip} =
  Object $
    HM.fromList
      [ ( "equipMstIds"
        , Array $
            V.fromList $
              fmap (Number . fromIntegral) $
                sort $ fmap slotId mstSlotitem
        )
      , ( "shipDigests"
        , Array $
            V.fromList $
              fmap
                (\shipId ->
                   Array $
                     V.fromList
                       [ Number (fromIntegral shipId)
                       , String (graphDigestTable IM.! shipId)
                       ])
                $ sort $ fmap (\KcShip {shipId} -> shipId) mstShip
        )
      ]
  where
    graphDigestTable :: IM.IntMap T.Text
    graphDigestTable = IM.fromList $ fmap mk mstShipgraph
      where
        mk KcShipgraph {shipId, filename, version} =
          (shipId, filename <> "#" <> NE.head version)
