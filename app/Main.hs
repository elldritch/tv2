module Main
  ( main
  )
where

import           System.IO                                ( IO
                                                          , putStrLn
                                                          )

import           Data.Function                            ( ($) )
import           Data.Int                                 ( Int )
import           Data.List                                ( (++) )
import qualified Data.Tree                     as Tree

import           Data.Maybe                               ( Maybe(..) )
import           Data.Either                              ( Either(..) )

import           Data.Semigroup                           ( (<>) )
import           Data.Functor                             ( fmap
                                                          , (<$>)
                                                          )

import           Text.Show                                ( show )

import qualified Data.Map.Strict               as Map

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
    Just level -> putStrLn $ case solve level of
      Right tree -> Tree.drawForest $ fmap (fmap show) tree
      Left  err  -> err
    Nothing -> putStrLn $ "Unknown level: " ++ (show n)
