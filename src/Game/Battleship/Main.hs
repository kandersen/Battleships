module Main where

import Game.Battleship.Core
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad
import Data.Map ((!), findMin, findMax, insert)
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do startGUI defaultConfig { tpPort = Just 80, tpAddr = Just (BS.pack $ "0.0.0.0") } setup

teamY = initBoard Unknown #
        insert (8,1) Miss #
        insert (3,2) Miss #
        insert (8,7) Miss #
        insert (8,4) Miss #
        insert (5,4) Miss #
        insert (4,5) Miss #
        insert (3,6) Miss #
        insert (6,3) Miss #
        insert (2,7) Miss #
        insert (2,3) Miss #
        insert (6,7) Miss

boardGP = frequencyMap [2,3,3,4,5] $
          initBoard Unknown #
          insert (3,6) Miss #
          insert (5,0) Miss #
          insert (5,1) Hit #
          insert (5,2) Hit #
          insert (5,3) Hit #
          insert (5,4) Miss #
          insert (6,1) Miss #
          insert (7,2) Miss #
          insert (8,1) Miss #
          insert (4,3) Miss 

setup :: Window -> UI ()
setup window = do
  return window # set title "BattleShips -- HeatMap"
  let
    redoLayout :: UI ()
    redoLayout = void $ do
      header1 <- UI.h1 #+ [string "Team Y"]
      layout1 <- renderHeatmap (frequencyMap [2,3,3,4,5] teamY)
      header2 <- UI.h1 #+ [string "Team Y - Aircraft Carrier Only"]
      layout2 <- renderHeatmap (frequencyMap [5] teamY)
      header3 <- UI.h1 #+ [string "Team GP"]
      layout3 <- renderHeatmap boardGP
      getBody window # set children [
        header1, layout1, header2, layout2, header3, layout3]

  redoLayout

type Color a = (a,a,a)

interpolate a (s,t,u) (x,y,z) = (i a s x, i a t y, i a u z)
  where 
    i a t0 t1 = t0 + a * (t1 - t0)
  
white = (255,255,255)
red = (255,0,0)
green = (0,255,0)
blue = (0,0,255)
yellow = (255,255,0)


color v = if v > 0.5
          then interpolate ((v - 0.5) * 2) yellow green
          else interpolate ((0.5 - v) * 2) yellow red

format (r,g,b) = "rgb(" ++ show (floor r) ++ "," ++ show (floor g) ++ "," ++ show (floor b) ++ ")"

renderHeatmap :: Board Int -> UI Element
renderHeatmap board = grid $
  map (\row ->
    map (\col ->
          let
            v = normalize board ! (row, col)
            c = color $ v in 
      (string $ "[" ++ take 4 (pad (show v)) ++ "]") # set UI.style
      [("background",format c)]
    ) [0..9]
  ) [0..9]
  where
    pad s = if length s == 3 then s ++ ['0'] else s
  
