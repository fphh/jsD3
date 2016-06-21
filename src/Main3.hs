{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)

import React.Flux


(.=) :: String -> String -> (Text, Aeson.Value)
a .= b = (Text.pack a) Aeson..= (Text.pack b)

data RGB = RGB Integer Integer Integer


newtype Style = Style { unStyle :: HashMap String String }
              deriving (Show)


classA = Style (HM.fromList[("color", "red")])
classB = Style (HM.fromList[("background-color", "yellow")])
classC = Style (HM.fromList[("opacity", "0.5")])

instance Monoid Style where
  mempty = Style HM.empty
  Style a `mappend` Style b = Style (a `mappend` b)


toStyle :: Style -> String
toStyle =
  List.intercalate ";"
  . map (\(x,y) -> x ++ ":" ++y)
  . HM.toList 
  . unStyle

(.++) :: (Sys.Var var, Monoid w, Monoid a) =>
       var w s a -> var w s a -> var w s a -> Sys.T w s ()
(.++) = Sys.assignment3 mappend

(.->) = ($)

equations = runST $ do
  x <- Sys.globalVariable
  Sys.solve $ do
    ca <- Sys.constant classA
    cb <- Sys.constant classB
    cc <- Sys.constant classC
    y <- Sys.localVariable
    ca .++ cb .-> y
    y .++ cc .-> x
  Sys.query x



main :: IO ()
main = print $ fmap toStyle equations
