module Player where

import Spell

data Player = Player
  { name :: String
  , hp :: Int
  , mp :: Int
  , spells :: [Spell]
  } deriving (Show)

newPlayer :: String -> Player
newPlayer name = Player name 10 10 []

cast :: Player -> Spell -> Maybe Int
cast player spell
  | cost spell <= mp player = Just $ cost spell
  | otherwise = Nothing

getRange :: Player -> Spell -> Maybe Spell
getRange player spell
  | cost spell < mp player = Just $ spell >> range (mp player - cost spell)
  | otherwise = Nothing

getPower :: Player -> Spell -> Maybe Spell
getPower player spell
  | cost spell < mp player = Just $ spell >> power (mp player - cost spell)
  | otherwise = Nothing

