{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TV2.Factories where

import           Data.Bool                                ( Bool(..) )

import qualified Data.Set                      as Set

import           TV2.Types                                ( Factory(..)
                                                          , Generator(..)
                                                          , Item(..)
                                                          )

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
