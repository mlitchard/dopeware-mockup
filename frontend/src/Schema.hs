{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Schema where
import Data.Map.Strict
import Data.Text (Text)

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
  deriving (Show, Enum, Eq, Ord)

data Planet = Planet {
      _resources :: Map ResourceName Resource
    , _neighbors :: [PlanetName]   
  } deriving (Show)

initPlanetMap :: Map PlanetName Planet
initPlanetMap = 
    fromList [(Arrakis,arrakis),(Minbar,minbar), (Tatooine, tatooine)
             , (CentauriPrime, centauriPrime), (Vulcan, vulcan)
             , (Dantooine, dantooine), (Mongo, mongo), (Terra,terra) 
             , (Pluto, pluto)]
    where
        -- No resource differentiation yet 
        partial       = Planet initResourceMap 
        arrakis       = partial [Arrakis, Minbar,CentauriPrime]
        minbar        = partial [Minbar, Arrakis, Vulcan, Tatooine]
        tatooine      = partial [Tatooine,Minbar, Dantooine]
        centauriPrime = partial [CentauriPrime, Arrakis, Vulcan]
        vulcan        = partial [Vulcan, Minbar, Dantooine
                                , Terra, CentauriPrime]
        dantooine     = partial [Dantooine, Tatooine, Pluto, Vulcan]
        mongo         = partial [Mongo, CentauriPrime, Terra]
        terra         = partial [Terra, Vulcan, Pluto, Mongo]
        pluto         = partial [Pluto, Dantooine, Terra] 
      
    

data ResourceName
  = FinestGreen
  | SubstanceD
  | Melange
  | PanGalacticGargleBlaster
  | BabyBlue
  | InterzoneSpecial
  deriving (Show, Enum, Eq, Ord)

data Resource = Resource {
      _highestPrice   :: PInt
    , _lowestPrice    :: PInt
    , _currentPrice   :: PInt
    , _count          :: PInt
    , _priceStability :: PriceStability   
  } deriving (Show)

rowOne, rowTwo, rowThree :: [PlanetName]
rowOne   = [Arrakis, Minbar, Tatooine]
rowTwo   = [CentauriPrime, Vulcan, Dantooine]
rowThree = [Mongo, Terra, Pluto]

initResourceMap :: Map ResourceName Resource
initResourceMap = fromList $ (\rn -> (rn,r)) <$> [FinestGreen .. InterzoneSpecial]
    where
        r = Resource 1000 10 500 10000 (Stable (PInt 50)) 
data PriceStability 
  = Volatile -- Time for a price change
  | Stable PInt -- Price change weight
  deriving (Show,Eq,Ord)

data Error
  = NotNeighbor
  deriving (Eq, Show)

data ScreenState = Travel | Market deriving Show

data PromptData = PromptData
    { _leftSide :: (Text,Text)
    , _rightSide :: (Text, Text)
    }
data GameState = GameState {
      _screenState :: ScreenState
    , _location    :: PlanetName
    , _error       :: Maybe (PlanetName, Error)
    , _planetMap   :: Map PlanetName Planet
    , _credits     :: Int
  } deriving (Show)
{-
data MapFormatting = MapFormatting {
      _rowOne   :: [PlanetName]
    , _rowTwo   :: [PlanetName]
    , _rowThree :: [PlanetName]
  } deriving (Show)
-}    
initGameState :: GameState
initGameState = GameState Travel Vulcan Nothing initPlanetMap 100 

updateScreen :: GameState -> ScreenState -> GameState
updateScreen gState screenState = gState {_screenState = screenState}
newtype PInt = PInt Int

instance Num PInt where

  x - y    = x `truncSub` y
                 where
                     truncSub (PInt x') (PInt y')
                       | y' > x'     = PInt 0
                       | otherwise = PInt (x' - y')

  x + y    = PInt (fromPInt x + fromPInt y)

  x * y    = PInt (fromPInt x * fromPInt y)

  abs x    = x

  signum _ = 1

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


