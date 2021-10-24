{-# LANGUAGE OverloadedStrings #-}

module Kantour.GameResource.Magic
  ( defaultServer
  , servers
  , magicCode
  )
where

import Data.Char
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

defaultServer :: T.Text
defaultServer = "203.104.209.71"

servers :: IM.IntMap T.Text
servers =
  IM.fromList
    [ (1, defaultServer)
    , (2, "203.104.209.87")
    , (3, "125.6.184.215")
    , (4, "203.104.209.183")
    , (5, "203.104.209.150")
    , (6, "203.104.209.134")
    , (7, "203.104.209.167")
    , (8, "203.104.209.199")
    , (9, "125.6.189.7")
    , (10, "125.6.189.39")
    , (11, "125.6.189.71")
    , (12, "125.6.189.103")
    , (13, "125.6.189.135")
    , (14, "125.6.189.167")
    , (15, "125.6.189.215")
    , (16, "125.6.189.247")
    , (17, "203.104.209.23")
    , (18, "203.104.209.39")
    , (19, "203.104.209.55")
    , (20, "203.104.209.102")
    ]

{- ORMOLU_DISABLE -}
resourceMagic :: VU.Vector Int
resourceMagic = VU.fromList
  [ 6657, 5699, 3371, 8909, 7719, 6229, 5449, 8561, 2987, 5501
  , 3127, 9319, 4365, 9811, 9927, 2423, 3439, 1865, 5925, 4409
  , 5509, 1517, 9695, 9255, 5325, 3691, 5519, 6949, 5607, 9539
  , 4133, 7795, 5465, 2659, 6381, 6875, 4019, 9195, 5645, 2887
  , 1213, 1815, 8671, 3015, 3147, 2991, 7977, 7045, 1619, 7909
  , 4451, 6573, 4545, 8251, 5983, 2849, 7249, 7449, 9477, 5963
  , 2711, 9019, 7375, 2201, 5631, 4893, 7653, 3719, 8819, 5839
  , 1853, 9843, 9119, 7023, 5681, 2345, 9873, 6349, 9315, 3795
  , 9737, 4633, 4173, 7549, 7171, 6147, 4723, 5039, 2723, 7815
  , 6201, 5999, 5339, 4431, 2911, 4435, 3611, 4423, 9517, 3243
  ]
{- ORMOLU_ENABLE -}

magicCode :: Int -> String -> Int
magicCode r seed =
  (17 * (r + 7) * (resourceMagic VU.! ((s + r * a) `rem` 100))) `rem` 8973 + 1000
  where
    s = getSum $ foldMap (Sum . ord) seed
    a = case seed of
      [] -> 1
      _ -> length seed
