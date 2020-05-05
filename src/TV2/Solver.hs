module TV2.Solver
  ( solve
  )
where

import           GHC.Num                                  ( (+) )

import           Control.Monad                            ( return )

import           Data.Function                            ( const
                                                          , ($)
                                                          )
import           Data.List                                ( (++) )
import           Data.String                              ( String )

import           Data.Either                              ( Either(..) )
import           Data.Either.Extra                        ( maybeToEither )

import           Data.Functor                             ( (<$>)
                                                          , fmap
                                                          )

import           Text.Show                                ( show )

import           Data.Set                                 ( Set )
import qualified Data.Set                      as Set

import           Data.Map.Strict                          ( Map )
import qualified Data.Map.Strict               as Map

import           Data.Tree                                ( Forest )
import qualified Data.Tree                     as Tree

import           TV2.Types                                ( Factory(..)
                                                          , Generator(..)
                                                          , Item(Worker)
                                                          , Level(..)
                                                          , Requirements
                                                          , Requirement
                                                          )

combineRequirements :: Requirements -> Requirements -> Requirements
combineRequirements = Map.unionWith (+)

solve :: Level -> Either String (Forest Requirement)
solve (Level { factories, generator, cities }) = reqs
 where
  allRequirements = Map.foldr combineRequirements Map.empty cities

  unfoldReqTree :: Requirement -> Either String (Requirement, [Requirement])
  unfoldReqTree req =
    (\rs -> (req, rs))
      <$> (ingredients (indexFactories factories) generator req)

  reqs = Tree.unfoldForestM unfoldReqTree (Map.toList allRequirements)

indexFactories :: Set Factory -> Map Item Factory
indexFactories factories = Map.fromList $ fmap toAssoc $ Set.toList factories
  where toAssoc factory = (output factory, factory)

ingredients
  :: Map Item Factory -> Generator -> Requirement -> Either String [Requirement]
ingredients factories (Generator { inputs = generatorInputs }) (item, count) =
  case item of
    Worker -> Right []
    _      -> do
      Factory { inputs = factoryInputs, powered } <-
        maybeToEither ("no factory found for " ++ show item)
          $ Map.lookup item factories
      let factoryTotals   = Map.fromSet (const count) factoryInputs
      let generatorTotals = Map.fromSet (const count) generatorInputs
      return $ Map.assocs $ combineRequirements
        factoryTotals
        (if powered then generatorTotals else Map.empty)
