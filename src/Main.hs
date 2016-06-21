{-# LANGUAGE TupleSections #-}

module Main where


import qualified UniqueLogic.ST.Rule as Rule
import qualified UniqueLogic.ST.System as Sys

import Control.Monad.ST (runST, ST)
import Control.Monad (liftM4, liftM3, unless)

import qualified Prelude as P
import Prelude hiding (max)


import qualified Data.Aeson as Aeson
import qualified Data.List as List


{- |
 > x=1
 > y=2
 > z=3
 > w=3

 > x+y=3
 > y*z=6
 > z=3
 > y^w=8
-}

example :: (Maybe Double, Maybe Double, Maybe Double, Maybe Double)
example =
   runST (do
         x <- Sys.globalVariable
         y <- Sys.globalVariable
         z <- Sys.globalVariable
         w <- Sys.globalVariable
         Sys.solve $ do
            c3 <- Sys.constant 3
            c6 <- Sys.constant 6
            c8 <- Sys.constant 8
            Rule.add x y c3
            Rule.mul y z c6
            Rule.equ z c3
            Rule.pow y w c8
         liftM4
            (,,,)
            (Sys.query x)
            (Sys.query y)
            (Sys.query z)
            (Sys.query w))



data Color = Green | Yellow | Red deriving (Show, Eq, Ord)

lampsTest :: Maybe Color
lampsTest = runST $ do
  x <- Sys.globalVariable
  y <- Sys.globalVariable
  z <- Sys.globalVariable
  Sys.solve $ do
    c0 <- Sys.constant Red
    c1 <- Sys.constant Green
    Rule.equ x c0
    Rule.equ y c1
    Rule.max x y z
  Sys.query z

data InOut = In | Out deriving (Show)

vars :: [(InOut, Lamp)] -> ST s [(InOut, Lamp, Sys.Variable w s Color)]
vars ls = mapM (\(io, l) -> fmap (io, l,) Sys.globalVariable) ls


{-
cst :: (Monoid w) =>
       [(InOut, Lamp, Sys.Variable w s Color)] -> Sys.T w s ()
cst ((In, Lamp color1, x):(In, Lamp color2, y):(Out, Lamp color3, z):_) = do
  c1 <- Sys.constant color1
  c2 <- Sys.constant color2
  Rule.equ x c1
  Rule.equ y c2
  Rule.max x y z
-}

cst :: (Monoid w) =>
       [(InOut, Lamp, Sys.Variable w s Color)] -> Sys.T w s ()
cst [] = return ()
cst ((In, Lamp color, x):xs) = do
  c <- Sys.constant color
  Rule.equ x c
  cst xs
cst ((Out, Lamp color, x):xs) = cst xs

max :: (Monoid w) =>
       [(InOut, Lamp, Sys.Variable w s Color)] -> Sys.T w s ()
max [] = return ()
max [_] = return ()
max [(_, _, x), (_, _, y)] = do
  c <- Sys.constant Green
  Rule.max c x y
max ((_, _, x):(_, _, y):xs@((_, _, z):_)) = do
  Rule.max x y z
  max xs

p (Out, _, c) = do
  x <- Sys.query c
  return (fmap Lamp x)
p (In, l, _) = return (Just l)




eqs ls = do
  vs <- vars ls
  Sys.solve $ do
    cst vs
    -- max vs
  mapM p vs


minR :: (Ord a, Sys.Var var, Monoid w) =>
       var w s a -> var w s a -> var w s a -> Sys.T w s ()
minR = Sys.assignment3 min


cs c1 c2 c3 = do
  let f (Just c) = Sys.constant c
      f Nothing = Sys.localVariable
  a <- f c1
  b <- f c2
  c <- f c3
  return (a, b, c)
  
test2 :: Maybe Color -> Maybe Color -> Maybe Color
      -> ST s (Maybe Color, Maybe Color, Maybe Color)
test2 c1 c2 c3 = do
  x <- Sys.globalVariable
  y <- Sys.globalVariable
  z <- Sys.globalVariable
  Sys.solve $ do
    (a, b, c) <- cs c1 c2 c3
        
    
    case c3 of
     Just _ -> do
       minR a c x
       minR b c y
       Rule.equ c z
     Nothing -> do
       Rule.equ a x
       Rule.equ b y
       Rule.max a b z
    
  liftM3 (,,) (Sys.query x) (Sys.query y) (Sys.query z)


data Lamp = Lamp Color
          deriving (Show)
                  
data ControlRoom = ControlRoom [Lamp] Lamp
                 deriving (Show)



test = [(In, Lamp Red), (Out, Lamp Yellow), (Out, Lamp Yellow)]

--setLamp :: (Ord a) => Color -> ST s (Maybe a)
--setLamp color = do
--  Sys.solve

main :: IO ()
main = do
  print (runST $ test2 (Just Yellow) (Just Green) Nothing)
  print (runST $ test2 (Just Red) (Just Green) (Just Green))
