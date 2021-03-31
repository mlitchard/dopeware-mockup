{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Schema where
import Data.Map.Strict

data PlanetName
  = Arrakis
  | Minbar
  | Tatooine
  | CentauriPrime
  | Vulcan
  | Dantooine
  | Mongo
  | Terra
  | Pluto -- Indulge my trolling please.
  deriving (Show, Enum, Eq)

data ResourceName
  = FinestGreen
  | SubstanceD
  | Melange
  | PanGalacticGargleBlaster
  | BabyBlue
  | InterzoneSpecial
  deriving (Show, Enum, Eq)


data PriceStability 
  = Volatile -- Time for a price change
  | Stable PInt -- Price change weight
  deriving (Show,Eq,Ord)

data Resource = Resource {
      _highestPrice   :: PInt
    , _lowestPrice    :: PInt
    , _currentPrice   :: PInt
    , _priceStability :: PriceStability   
  } deriving (Show)

data GameState = GameState {
      _location  :: PlanetName
    , _credits   :: Int
    , _resources :: Map ResourceName Resource
  } deriving (Show)

data MapFormatting = MapFormatting {
      _rowOne   :: [(PlanetName, Bool)]
    , _rowTwo   :: [(PlanetName, Bool)]
    , _rowThree :: [(PlanetName, Bool)]
  } deriving (Show)
    
initGameState :: GameState
initGameState = GameState Arrakis 100 empty
newtype PInt = PInt Int

instance Num PInt where

  x - y = x `truncSub` y
            where
              truncSub (PInt x) (PInt y)
                | y > x     = PInt 0
                | otherwise = PInt (x - y)

  x + y = PInt (fromPInt x + fromPInt y)

  x * y = PInt (fromPInt x * fromPInt y)

  abs x = x

  signum x = 1

  fromInteger x = PInt (fromInteger x)

instance Eq PInt where
   x == y = fromPInt x == fromPInt y
   x /= y = fromPInt x /= fromPInt y

instance Ord PInt where
   x <= y = fromPInt x <= fromPInt y
   x < y         = fromPInt x < fromPInt y
   x > y         = fromPInt x > fromPInt y

instance Show PInt where
   show x = show $ fromPInt x

fromPInt :: PInt -> Int
fromPInt (PInt a) = a

