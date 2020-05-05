module TV2.Types
  ( Item(..)
  , Level(..)
  , Factory(..)
  , Generator(..)
  , City
  , Requirements
  , Requirement
  )
where


import           Data.Bool                                ( Bool(..) )
import           Data.Int                                 ( Int )
import           Data.String                              ( String )

import           Data.Eq                                  ( Eq )
import           Data.Ord                                 ( Ord )
import           Text.Show                                ( Show )

import           Data.Map.Strict                          ( Map )
import           Data.Set                                 ( Set )

data Item =
    Worker
  | Coal
  | CopperOre
  | CopperIngots
  | Polymers
  | Wire
  | Oil
  | Glass
  | Sand
  | SteelSlab
  | SteelPlate
  | BuildingMaterials
  | IronOre
  | Appliances
  | NaturalRubber
  | ProcessedRubber
  deriving (Eq, Ord, Show)

data Factory = Factory {
  powered :: Bool,
  inputs  :: Set Item,
  output  :: Item
} deriving (Eq, Ord)

data Generator = Generator {
  inputs :: Set Item
}

type City = String

type Requirements = Map Item Int

type Requirement = (Item, Int)

data Level = Level {
  factories :: Set Factory,
  generator :: Generator,
  cities    :: Map City Requirements
}
