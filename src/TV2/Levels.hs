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
  Factory { powered = False, inputs = Set.singleton Worker, output = Sand }

ironOre :: Factory
ironOre =
  Factory { powered = False, inputs = Set.singleton Worker, output = IronOre }

steelSlab :: Factory
steelSlab = Factory { powered = False
                    , inputs  = Set.fromList [Worker, Coal, IronOre]
                    , output  = SteelSlab
                    }

steelPlate :: Factory
steelPlate = Factory { powered = True
                     , inputs  = Set.fromList [Worker, SteelSlab]
                     , output  = SteelPlate
                     }

buildingMaterials :: Factory
buildingMaterials = Factory { powered = True
                            , inputs  = Set.fromList [Worker, SteelPlate, Glass]
                            , output  = BuildingMaterials
                            }

appliances :: Factory
appliances = Factory
  { powered = False
  , inputs  = Set.fromList [Worker, CopperIngots, ProcessedRubber]
  , output  = Appliances
  }

naturalRubber :: Factory
naturalRubber = Factory { powered = False
                        , inputs  = Set.singleton Worker
                        , output  = NaturalRubber
                        }

processedRubber :: Factory
processedRubber = Factory { powered = False
                          , inputs  = Set.fromList [Worker, NaturalRubber]
                          , output  = ProcessedRubber
                          }

-- Shipyards.

copperIngotShipyard :: Factory
copperIngotShipyard = Factory { powered = False
                              , inputs  = Set.singleton Worker
                              , output  = CopperIngots
                              }

-- Default generators.

coalGenerator :: Generator
coalGenerator = Generator { inputs = Set.fromList [Worker, Coal] }

hydroGenerator :: Generator
hydroGenerator = Generator { inputs = Set.singleton Worker }

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

goldenGate :: Level
goldenGate = Level
  { factories = Set.fromList
                  [ ironOre
                  , coal
                  , steelSlab
                  , buildingMaterials
                  , steelPlate
                  , sand
                  , glass
                  ]
  , generator = coalGenerator
  , cities    = Map.fromList
                  [ ("south", Map.fromList [(Glass, 12)])
                  , ( "northwest"
                    , Map.fromList
                      [(SteelSlab, 8), (BuildingMaterials, 5), (SteelPlate, 5)]
                    )
                  ]
  }

wardenclyffe :: Level
wardenclyffe = Level
  { factories = Set.fromList
                  [ copperIngotShipyard
                  , wire
                  , oil
                  , polymers
                  , appliances
                  , naturalRubber
                  , processedRubber
                  ]
  , generator = hydroGenerator
  , cities    = Map.fromList
                  [ ("docks", Map.fromList [(Appliances, 12), (Polymers, 4)])
                  , ("woods", Map.fromList [(Wire, 12), (ProcessedRubber, 8)])
                  ]
  }

-- TODO: Really this should be a list, but I don't have all the level data.
levels :: Map Int Level
levels = Map.fromList [(25, panamaCanal), (26, goldenGate), (27, wardenclyffe)]
