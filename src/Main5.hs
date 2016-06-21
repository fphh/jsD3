{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

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


import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Concurrent

import GHCJS.Types
import GHCJS.Prim
import GHCJS.Marshal
import GHCJS.Marshal.Pure

import JavaScript.Object


type Label = String

data Color = Green | Yellow | Red | Blue deriving (Show, Eq, Ord, Enum)

data Direction = X Double | Y Double | Z Double deriving (Generic, NFData)

data Action =
  Init
  | Click Direction deriving (Generic, NFData)

type Lines a = [[Coord a]]

data AppState = AppState {
  thetas :: (Double, Double, Double),
  lines :: Lines Double,
  strLines :: [String],
  axes :: [(Label, Coord Double, Coord Double)]
  } deriving (Show)


foreign import javascript safe
  "$r = line($1)" line :: JSVal -> IO JSVal

testf :: Double -> Double -> Coord Double
testf x z = Coord x (abs $ x*z / (x^2 + 1)) z 0

testf2 :: Double -> Double -> Coord Double
testf2 x z = Coord x (sin x) z 0

testf3 :: Double -> Double -> Coord Double
testf3 x z = Coord x 0 z 0

testf4 :: Double -> Double -> Coord Double
testf4 z x = Coord x (sin x * (log (1+abs z))^2) z 0


testf5 :: Double -> Double -> Coord Double
testf5 z x = Coord x (x*z/20) z 0


matrix :: (Double -> Double -> Coord Double) -> Lines Double
matrix f = map f zs >>= \h -> [map h xs]
  where xs = [-7, -6.6 .. 7]
        zs = [-7, -6.6 .. 7]

coordToPairs :: [Coord Double] -> IO JSVal
coordToPairs xs =
  mapM (\(Coord x y _ _) -> toJSArray [pToJSVal x, pToJSVal y]) xs
  >>= toJSArray

coordToJSVal :: [[Coord Double]] -> IO [String]
coordToJSVal xs =
  mapM coordToPairs xs
  >>= sequence . map (fmap pFromJSVal . line)

scaleXYZ :: (Num a) => a -> Coord a -> Coord a
scaleXYZ f (Coord a b c d) = Coord (f*a) (f*b) (f*c) (f*d)

scale :: (Num a) => a -> Lines a -> Lines a
scale f = map (map (scaleXYZ f))

instance StoreData AppState where
    type StoreAction AppState = Action

    transform Init (AppState th cs _ as) = do
      ls <- coordToJSVal (cs ++ List.transpose cs)
      return (AppState th cs ls as)

    transform (Click dir) (AppState (thx, thy, thz) cs _ as) = do
      let trans = transformCoord rotMat
          ds = map (map trans) cs
          (th, rotMat) =
            case dir of
             X th -> ((th, thy, thz), rotationX (thx-th))
             Y th -> ((thx, th, thz), rotationY (thy-th))
             Z th -> ((thx, thy, th), rotationZ (thz-th))
          f (l, a, b) = (l, trans a, trans b)
      ls <- coordToJSVal (ds ++ List.transpose ds)
      return (AppState th ds ls (map f as))


xaxis, yaxis, zaxis :: (Label, Coord Double, Coord Double)
xaxis = ("X", Coord (-300) 0 0 0, Coord 300 0 0 0)
yaxis = ("Y", Coord 0 (-300) 0 0, Coord 0 300 0 0)
zaxis = ("Z", Coord 0 0 (-300) 0, Coord 0 0 300 0)

store :: ReactStore AppState
store = mkStore (AppState (0, 0, 0) t [] [xaxis, yaxis, zaxis])
  where t = scale 30 $ matrix testf4
                 
pack :: Show a => a -> Text.Text
pack = Text.pack . show

color :: Color -> String
color c =
  case c of
   Red -> "#ff0000"
   Yellow -> "#ffff00"
   Green -> "#008800"
   Blue -> "#0000aa"
   
toFill :: Color -> Aeson.Value
toFill c = Aeson.object [ "fill" Aeson..= color c ]

toStroke :: Color -> Double -> Aeson.Value
toStroke c sw =
  Aeson.object [ "stroke" Aeson..= color c
               , "strokeWidth" Aeson..= sw ]

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

translate :: Coord Double -> [PropertyOrHandler handler]
translate (Coord x y _ _) =
  [ "transform" $= (Text.pack $ "translate(" ++ show x ++ ", " ++ show y ++ ")") ]


drawPaths :: [String] -> ReactElementM ViewEventHandler ()
drawPaths xs =
  let
    f :: String -> ReactElementM ViewEventHandler ()
    f str = path_ [ "d" $= Text.pack str ] mempty
  in mapM_ f xs

drawAxis :: Color -> (Label, Coord Double, Coord Double) -> ReactElementM ViewEventHandler ()
drawAxis color (l, Coord a b _ _, Coord x y _ _) = do
  line_ [ "x1" $= pack a
        , "y1" $= pack b
        , "x2" $= pack x
        , "y2" $= pack y
        , "style" @= toStroke color 1] mempty
  text_ [ "x" $= pack a, "y" $= pack b, "style" @= toFill color ] (elemText l)
  
render :: AppState -> ReactElementM ViewEventHandler ()
render (AppState _ _ ls axes) = do
  let f dir = [ "className" $= "button"
              , onClick $ \evt _ -> dispatch (Click dir) ]
      w = 1200
      h = 640
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
    circle_ [ "style" @= toStroke Red 1
            , "x" $= pack (-600)
            , "y" $= pack (-300)
            , "r" $= pack 10 ] mempty
    drawAxis Red xaxis
    drawAxis Red yaxis
    drawPaths ls
    mapM_ (drawAxis Green) axes

app :: ReactView ()
app = defineControllerView "app" store $ \state () -> render state
  
main :: IO ()
main = do
  executeAction (SomeStoreAction store Init)  
  reactRender "app" app ()
