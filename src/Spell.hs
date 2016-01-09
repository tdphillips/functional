{-# Language DeriveFunctor #-}
module Spell where

import Control.Monad.Free

-- | 10 is the magic number for everything

data CastType
  = Beam
  | BlastRadius
  | Target
  deriving (Show)

data Element
  = Earth -- knocked down, no movement
  | Wind -- slowed, half action points
  | Water -- frozen, skip turn
  | Fire -- burning, damage over time
  deriving (Show)

data SpellF next
  = CastType CastType next
  | Element Element next
  | Range Int next
  | Power Int next
  deriving (Show, Functor)

type Spell' a = Free SpellF a

type Spell = Spell' ()

beam :: Spell
beam = liftF $ CastType Beam ()

blastRadius :: Spell
blastRadius = liftF $ CastType BlastRadius ()

target :: Spell
target = liftF $ CastType Target ()

earth :: Spell
earth = liftF $ Element Earth ()

wind :: Spell
wind = liftF $ Element Wind ()

water :: Spell
water = liftF $ Element Water ()

fire :: Spell
fire = liftF $ Element Fire ()

range :: Int -> Spell
range r = liftF $ Range r ()

power :: Int -> Spell
power p = liftF $ Power p ()

fireBeam :: Spell
fireBeam = fire >> beam

cost :: Spell -> Int
cost (Free (CastType Beam next)) = 3 + cost next
cost (Free (CastType BlastRadius next)) = 6 + cost next
cost (Free (CastType Target next)) = 4 + cost next
cost (Free (Element Earth next)) = 6 + cost next
cost (Free (Element Wind next)) = 3 + cost next
cost (Free (Element Water next)) = 4 + cost next
cost (Free (Element Fire next)) = 5 + cost next
cost (Free (Range value next)) = value + cost next
cost (Free (Power value next)) = 2 * value + cost next
cost (Pure next) = 0
