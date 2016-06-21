{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

import qualified UniqueLogic.ST.Rule as Rule
import qualified UniqueLogic.ST.System as Sys

import Control.Monad.ST (runST, ST)
import Control.Monad (liftM4, liftM3, unless, mapM_)

import qualified Prelude as P
import Prelude hiding (max)


import qualified Data.Aeson as Aeson
import qualified Data.List as List

import qualified Data.Text as Text

import React.Flux

import Data.Tree

import Transform (Coord(..), transformCoord, rotationX, rotationY, rotationZ, mapCoord)

import Debug.Trace

bigTree :: Tree ()
bigTree = Node () (repeat bigTree)

depth :: Int -> Tree a -> Tree a
depth 1 (Node x ts) = Node x []
depth n (Node x ts) = Node x (take 2 $ map (depth (n-1)) ts)

data Color = Green | Yellow | Red | Blue deriving (Show, Eq, Ord, Enum)

data Direction = X Double | Y Double | Z Double deriving (Generic, NFData)

data Action = Click Direction deriving (Generic, NFData)


{-
instance Functor Coord where
  fmap f (Coord x y) = Coord (f x) (f y)
-}

  
data AppState = AppState {
  thetas :: (Double, Double, Double),
  tree :: Tree (Coord Double, ())
  } deriving (Show)


instance StoreData AppState where
    type StoreAction AppState = Action

    transform (Click dir) (AppState (thx, thy, thz) t) = do
      let s = fmap (mapCoord (transformCoord rotMat)) t
          (th, rotMat) =
            case dir of
             X th -> ((th, thy, thz), rotationX (thx-th))
             Y th -> ((thx, th, thz), rotationY (thy-th))
             Z th -> ((thx, thy, th), rotationZ (thz-th))
      print th
      return (AppState th s)

color :: Color -> String
color c =
  case c of
   Red -> "#ff0000"
   Yellow -> "#ffff00"
   Green -> "#00ff00"
   Blue -> "#0000aa"
   
toFill :: Color -> Aeson.Value
toFill c = Aeson.object [ "fill" Aeson..= color c ]

toStroke :: Color -> Double -> Aeson.Value
toStroke c sw =
  Aeson.object [ "stroke" Aeson..= color c
               , "strokeWidth" Aeson..= sw ]

store :: ReactStore AppState
store = mkStore (AppState (0, 0, 0) t)
  where t = layoutTree 0 (-200, 200) (depth 5 bigTree)
                 
dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]


levelDistance :: Double
levelDistance = 50

layoutTree :: Integer -> (Double, Double) -> Tree a -> Tree (Coord Double, a)
layoutTree depth (from, to) (Node x []) =
  Node (Coord (from + (to-from) / 2) (fromIntegral depth * levelDistance) 0 1, x) []
  
layoutTree depth (from, to) (Node x ts) =
  Node (Coord (from + (to-from) / 2) (fromIntegral depth * levelDistance) 0 1, x) vs
  where
    vs = zipWith (\x t -> layoutTree (depth+1) (newFrom x, newTo x) t) [0..] ts
    intervalWidth = (to-from) / fromIntegral (length ts)
    newFrom x = from + x*intervalWidth
    newTo x = from + (x+1)*intervalWidth


translate :: Coord Double -> [PropertyOrHandler handler]
translate (Coord x y _ _) =
  [ "transform" $= (Text.pack $ "translate(" ++ show x ++ ", " ++ show y ++ ")") ]

paintNodes :: Tree (Coord Double, a) -> ReactElementM ViewEventHandler ()
paintNodes (Node (Coord x y z _, l) ts) = do
  circle_ [ "cx" $= pack x
          , "cy" $= pack y
          , "r" $= pack (2 + ((z+300)/300)*3 )
          , "style" @= toFill Red ] $ return ()
  mapM_ paintNodes ts

pack = Text.pack . show

paintLines :: Tree (Coord Double, a) -> ReactElementM ViewEventHandler ()
paintLines (Node (Coord a b _ _, l) ts) = do
  mapM_ f (map (fst . rootLabel) ts)
  mapM_ paintLines ts
  where
    f (Coord x y z _) =
      line_ [ "x1" $= pack a
            , "x2" $= pack x
            , "y1" $= pack b
            , "y2" $= pack y
            , "style" @= toStroke Blue (2*((z+300)/300)) ] $ return ()
            
render :: AppState -> ReactElementM ViewEventHandler ()
render (AppState _ lt) = do
  let f dir = [ "className" $= "button", onClick $ \evt _ -> dispatch (Click dir) ]
      w = 1200
      h = 600
      vb = Text.pack $
           show (-w/2) ++ " " ++ show (-h/2) ++ " " ++ show w ++ " " ++ show h
      input for label dir =
        labeledInput_ for label
        [ "type" $= "range"
        , "min" $= pack 0
        , "max" $= pack (2*pi)
        , "step" $= pack (pi/100)
        , "defaultValue" $= pack 0
        , onChange $ \evt -> dispatch (Click (dir (target evt "value"))) ]
  div_ [] $ do
    input "x-rotate" "Rotate around X axis" X
    input "y-rotate" "Rotate around Y axis" Y
    input "z-rotate" "Rotate around Z axis" Z
  svg_ [ "width" $= pack w, "height" $= pack h, "viewBox" $= vb ] $ do
    circle_ [ "style" @= toFill Green
            , "x" $= pack (-600)
            , "y" $= pack (-300)
            , "r" $= pack 10 ] mempty
    paintLines lt
    paintNodes lt


app :: ReactView ()
app = defineControllerView "app" store $ \state () -> render state
  
main :: IO ()
main = do
--   print (layoutTree 0 (0, 800) (depth 2 bigTree))
  reactRender "app" app ()

{-
main :: IO ()
main = do
  print (equations test1)
  print (equations test2)
-}
