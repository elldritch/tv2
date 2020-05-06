{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TV2.Levels where

import           Data.Int                                 ( Int )

import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as Map

import qualified Data.Set                      as Set

import           TV2.Types                                ( Item(..)
                                                          , Level(..)
                                                          )
import           TV2.Factories

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
                  , steelPlates
                  , sand
                  , glass
                  ]
  , generator = coalGenerator
  , cities    = Map.fromList
                  [ ("south", Map.fromList [(Glass, 12)])
                  , ( "northwest"
                    , Map.fromList
                      [(SteelSlab, 8), (BuildingMaterials, 5), (SteelPlates, 5)]
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

maeklong :: Level
maeklong = Level
  { factories = Set.fromList
                  [ grain
                  , cattle
                  , steelPlateShipyard
                  , sand
                  , coalTimed
                  , glass
                  , cannedGoods
                  , buildingMaterials
                  ]
  , generator = coalGenerator
  , cities    = Map.fromList
                  [ ("southwest", Map.fromList [(Cattle, 8)])
                  , ( "southeast"
                    , Map.fromList [(Glass, 12), (BuildingMaterials, 10)]
                    )
                  , ("northwest", Map.fromList [(CannedGoods, 10)])
                  ]
  }

-- TODO: Really this should be a list, but I don't have all the level data.
levels :: Map Int Level
levels = Map.fromList
  [(25, panamaCanal), (26, goldenGate), (27, wardenclyffe), (28, maeklong)]
