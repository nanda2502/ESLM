module Params where

data Params = Params {
    n :: Int,
    num_nodes :: Int,
    edge_mean :: Double,
    edge_sd :: Double,
    edge_threshold :: Double,
    agent_edge_mean :: Double,
    agent_edge_sd :: Double
} deriving (Show, Eq)

