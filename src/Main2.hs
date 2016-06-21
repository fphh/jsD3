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
import Control.Monad (liftM4, liftM3, unless)

import qualified Prelude as P
import Prelude hiding (max)


import qualified Data.Aeson as Aeson
import qualified Data.List as List

import React.Flux


data Color = Green | Yellow | Red deriving (Show, Eq, Ord, Enum)

minR :: (Ord a, Sys.Var var, Monoid w) =>
       var w s a -> var w s a -> var w s a -> Sys.T w s ()
minR = Sys.assignment3 min

cs :: (Monoid w)
      => Maybe a
      -> Maybe a
      -> Maybe a
      -> Sys.T w s (Sys.Variable w s a, Sys.Variable w s a, Sys.Variable w s a)

cs c1 c2 c3 = do
  let f (Just c) = Sys.constant c
      f Nothing = Sys.localVariable
  a <- f c1
  b <- f c2
  c <- f c3
  return (a, b, c)

(.=) :: (Monoid w) => Sys.Variable w s a -> Sys.Variable w s a -> Sys.T w s ()
(.=) = Rule.equ

  
equations :: (Ord a) => AppState a -> AppState a
equations (AppState c1 c2 c3) = runST $ do
  x <- Sys.globalVariable
  y <- Sys.globalVariable
  z <- Sys.globalVariable
  Sys.solve $ do
    (a, b, c) <- cs c1 c2 c3
    
    case c3 of
     Just _ -> do
       minR a c x
       minR b c y
       z .= c
     Nothing -> do
       x .= a
       y .= b
       Rule.max a b z
    
  liftM3 AppState (Sys.query x) (Sys.query y) (Sys.query z)


data AppState a = AppState (Maybe a) (Maybe a) (Maybe a)
              deriving (Show)

instance Functor AppState where
  fmap f (AppState a b c) = AppState (fmap f a) (fmap f b) (fmap f c)

test1 = AppState (Just Yellow) (Just Green) Nothing
test2 = AppState (Just Red) (Just Green) (Just Yellow)

toStyle :: Maybe Color -> Aeson.Value
toStyle color = Aeson.object [ "backgroundColor" Aeson..= c ]
  where
    c :: String
    c = case color of
         Just Red -> "#ff0000"
         Just Yellow -> "#ffff00"
         Just Green -> "#00ff00"
         Nothing -> "#999999"

data Action = Click Int deriving (Generic, NFData)


pre Green = Green
pre Yellow = Green
pre Red = Yellow

suc Green = Yellow
suc Yellow = Red
suc Red = Red

instance StoreData (AppState Color) where
    type StoreAction (AppState Color) = Action

    transform (Click n) (AppState x y z) =
      return
      $ equations
      $ case n of
         0 -> AppState (fmap suc x) y Nothing
         1 -> AppState x (fmap suc y) Nothing
         2 -> AppState x y (fmap pre z)


store :: ReactStore (AppState Color)
store = mkStore $ equations test2

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

render :: AppState Color -> ReactElementM ViewEventHandler ()
render (AppState x y z) = do
  div_ [ "style" @= toStyle x
       , "className" $= "lamp"
       , onClick $ \_ _ -> dispatch (Click 0) ] (elemText "Asset 1")
  div_ [ "style" @= toStyle y
       , "className" $= "lamp"  
       , onClick $ \_ _ -> dispatch (Click 1) ] (elemText "Asset 2")
  div_ [ "style" @= toStyle z
       , "className" $= "lamp"  
       , onClick $ \_ _ -> dispatch (Click 2) ] (elemText "Overall State")
  
        
app :: ReactView ()
app = defineControllerView "app" store $ \state () -> render state
  
main :: IO ()
main = reactRender "app" app ()

{-
main :: IO ()
main = do
  print (equations test1)
  print (equations test2)
-}
