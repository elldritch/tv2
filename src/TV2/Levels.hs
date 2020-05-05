{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TV2.Levels where

import           Data.Bool                                ( Bool(..) )
import           Data.Int                                 ( Int )

import           TV2.Types                                ( Factory(..)
                                                          , Generator(..)
                                                          , Item(..)
                                                          , Level(..)
                                                          )

import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as Map

import qualified Data.Set                      as Set


-- Default factories.

copperOre :: Factory
copperOre =
  Factory { powered = False, inputs = Set.singleton Worker, output = CopperOre }

coal :: Factory
coal =
  Factory { powered = False, inputs = Set.singleton Worker, output = Coal }

oil :: Factory
oil = Factory { powered = False, inputs = Set.singleton Worker, output = Oil }

copperIngots :: Factory
copperIngots = Factory { powered = False
                       , inputs  = Set.fromList [Worker, Coal, CopperOre]
                       , output  = CopperIngots
                       }

polymers :: Factory
polymers = Factory { powered = True
                   , inputs  = Set.fromList [Worker, Oil]
                   , output  = Polymers
                   }

wire :: Factory
wire = Factory { powered = True
               , inputs  = Set.fromList [Worker, CopperIngots, Polymers]
               , output  = Wire
               }

glass :: Factory
glass = Factory { powered = False
                , inputs  = Set.fromList [Worker, Sand]
                , output  = Glass
                }

sand :: Factory
sand =
  Factory { powered = False, inputs = Set.fromList [Worker], output = Sand }

-- Default generators.

coalGenerator :: Generator
coalGenerator = Generator { inputs = Set.fromList [Worker, Coal] }

-- Levels.

panamaCanal :: Level
panamaCanal = Level
  { factories = Set.fromList
                  [copperOre, coal, polymers, oil, copperIngots, wire]
  , generator = coalGenerator
  , cities    = Map.fromList
                  [ ("south", Map.fromList [(CopperIngots, 12), (Wire, 5)])
                  , ("north", Map.fromList [(Polymers, 10), (Wire, 5)])
                  ]
  }

-- TODO: Really this should be a list, but I don't have all the level data.
levels :: Map Int Level
levels = Map.fromList [(25, panamaCanal)]
