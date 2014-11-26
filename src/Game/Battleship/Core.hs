module Game.Battleship.Core where

import Data.Map (Map, (!))
import qualified Data.Map as Map

data Cell = Hit
          | Miss
          | Unknown
          deriving (Show, Eq)

type Position = (Int, Int)

type Board a = Map Position a

positions :: [Position]
positions = [ (x,y) | x <- [0..9], y <- [0..9]]

initBoard :: a -> Board a
initBoard a = Map.fromList [(ix,a) | ix <- positions ]

inbounds :: Position -> Bool
inbounds (r,c) =
  0 <= r && r <= 9 && 0 <= c && c <= 9

arrangements :: Int -> Position -> [[Position]]
arrangements shipLength (r,c) = 
  [map (\n -> (r + n, c)) [0..shipLength - 1],
   map (\n -> (r - n, c)) [0..shipLength - 1],
   map (\n -> (r, c + n)) [0..shipLength - 1],
   map (\n -> (r, c - n)) [0..shipLength - 1]]

waysToPlace :: Int -> Board Cell -> Position -> Cell -> Int
waysToPlace _ _ _ Miss = 0
waysToPlace shipLength board pos _ =
  length $
  filter (all ((/=Miss) . (board!))) $
  filter (all inbounds) $
  arrangements shipLength pos

frequencyMapForShip :: Board Cell -> Int -> Board Int
frequencyMapForShip board shipLength = Map.mapWithKey (waysToPlace shipLength board) board

frequencyMap :: [Int] -> Board Cell -> Board Int
frequencyMap ships board = Map.unionsWith (+) $ map (frequencyMapForShip board) ships

normalize :: Board Int -> Board Double
normalize board = fmap ((/span) . (flip (-) low) . fromIntegral) board
  where
    es = Map.elems board
    high = fromIntegral $ maximum es
    low = fromIntegral $ minimum es
    span = high - low
    
