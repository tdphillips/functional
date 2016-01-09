module Map where

import Player

type Map = [[Tile]]

data Tile = EmptyTile | PlayerTile Player deriving (Show)

newMap :: Player -> Player -> Map
newMap host guest = 
  [ playerRow guest
  , emptyRow
  , emptyRow
  , emptyRow
  , playerRow host
  ]
  where
    playerRow p = [EmptyTile, EmptyTile, PlayerTile p, EmptyTile, EmptyTile]
    emptyRow = replicate 5 EmptyTile

