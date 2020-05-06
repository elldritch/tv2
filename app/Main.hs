module Main
  ( main
  )
where

import           System.IO                                ( IO
                                                          , putStrLn
                                                          )

import           Data.Int                                 ( Int )
import           GHC.Num                                  ( (+) )
import           Data.Tuple                               ( snd )
import           Data.Function                            ( ($) )

import           Data.List                                (intercalate,  (++), sortOn, reverse )
import qualified Data.Tree                     as Tree
import           Data.Tree                                ( Forest
                                                          , Tree
                                                          )
import qualified Data.Map.Strict               as Map

import           Data.Maybe                               ( Maybe(..) )
import           Data.Either                              ( Either(..) )

import           Data.Semigroup                           ( (<>) )
import           Data.Foldable                            ( foldr )
import           Data.Functor                             ( fmap
                                                          , (<$>)
                                                          )

import           Text.Show                                ( show )

import           Options.Applicative                      ( ParserInfo
                                                          , auto
                                                          , briefDesc
                                                          , execParser
                                                          , helper
                                                          , info
                                                          , long
                                                          , option
                                                          , progDesc
                                                          , short
                                                          , (<**>)
                                                          )

import           TV2.Types
import           TV2.Levels                               ( levels )
import           TV2.Solver                               ( solve )

data Opts = Opts {
  levelNumber :: Int
}

opts :: ParserInfo Opts
opts = info (options <**> helper)
            (briefDesc <> progDesc "A calculator for Train Valley 2 levels")
  where options = Opts <$> option auto (long "level" <> short 'l')

main :: IO ()
main = do
  options <- execParser opts
  let n = levelNumber options
  case Map.lookup n levels of
    Nothing    -> putStrLn $ "Unknown level: " ++ (show n)
    Just level -> case solve level of
      Left  err  -> putStrLn err
      Right tree -> do
        putStrLn $ Tree.drawForest $ fmap (fmap show) tree
        putStrLn
          $ intercalate "\n"
          $ fmap (\(item, count) -> (show item) ++ ": " ++ (show count))
          $ reverse $ sortOn snd
          $ Map.toList $ treeTotals tree

 where
  treeTotals :: (Forest Requirement) -> Requirements
  treeTotals forest = foldr accForest Map.empty forest

  accForest :: Tree Requirement -> Requirements -> Requirements
  accForest tree acc = Map.unionWith (+) (foldTree tree) acc

  foldTree :: Tree Requirement -> Requirements
  foldTree tree = foldr accTree Map.empty tree

  accTree :: (Requirement -> Requirements -> Requirements)
  accTree (item, count) acc = Map.alter (addReq count) item acc

  addReq :: Int -> Maybe Int -> Maybe Int
  addReq n v = case v of
    Just v' -> Just (n + v')
    Nothing -> Just n
