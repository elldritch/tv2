module Main
  ( main
  )
where

import           System.IO                                ( IO )

import           Data.Function                            ( ($) )
import           Data.Tree                                ( Forest
                                                          , Tree(..)
                                                          )
import           Data.Either                              ( Either(..) )

import           Test.Hspec                               ( hspec
                                                          , describe
                                                          , it
                                                          , shouldBe
                                                          )

import           TV2.Types                                ( Requirement
                                                          , Item(..)
                                                          )
import           TV2.Levels                               ( panamaCanal )
import           TV2.Solver                               ( solve )


panamaCanalAnswer :: Forest Requirement
panamaCanalAnswer =
  [ Node
    { rootLabel = (CopperIngots, 12)
    , subForest =
      [ Node { rootLabel = (Worker, 12), subForest = [] }
      , Node { rootLabel = (Coal, 12)
             , subForest = [Node { rootLabel = (Worker, 12), subForest = [] }]
             }
      , Node { rootLabel = (CopperOre, 12)
             , subForest = [Node { rootLabel = (Worker, 12), subForest = [] }]
             }
      ]
    }
  , Node
    { rootLabel = (Polymers, 10)
    , subForest =
      [ Node { rootLabel = (Worker, 20), subForest = [] }
      , Node { rootLabel = (Coal, 10)
             , subForest = [Node { rootLabel = (Worker, 10), subForest = [] }]
             }
      , Node { rootLabel = (Oil, 10)
             , subForest = [Node { rootLabel = (Worker, 10), subForest = [] }]
             }
      ]
    }
  , Node
    { rootLabel = (Wire, 10)
    , subForest =
      [ Node { rootLabel = (Worker, 20), subForest = [] }
      , Node { rootLabel = (Coal, 10)
             , subForest = [Node { rootLabel = (Worker, 10), subForest = [] }]
             }
      , Node
        { rootLabel = (CopperIngots, 10)
        , subForest =
          [ Node { rootLabel = (Worker, 10), subForest = [] }
          , Node
            { rootLabel = (Coal, 10)
            , subForest = [Node { rootLabel = (Worker, 10), subForest = [] }]
            }
          , Node
            { rootLabel = (CopperOre, 10)
            , subForest = [Node { rootLabel = (Worker, 10), subForest = [] }]
            }
          ]
        }
      , Node
        { rootLabel = (Polymers, 10)
        , subForest =
          [ Node { rootLabel = (Worker, 20), subForest = [] }
          , Node
            { rootLabel = (Coal, 10)
            , subForest = [Node { rootLabel = (Worker, 10), subForest = [] }]
            }
          , Node
            { rootLabel = (Oil, 10)
            , subForest = [Node { rootLabel = (Worker, 10), subForest = [] }]
            }
          ]
        }
      ]
    }
  ]


main :: IO ()
main = hspec $ do
  describe "Solver" $ do
    it "produces recipe trees for levels" $ do
      solve panamaCanal `shouldBe` Right panamaCanalAnswer
