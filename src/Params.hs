module Params where

data Params = Params {
    n :: Int,
    num_nodes :: Int,
    edge_mean :: Double,
    edge_sigma :: Double,
    edge_threshold :: Double,
    agent_edge_mean :: Double,
    agent_edge_sigma :: Double
} deriving (Show, Eq)

