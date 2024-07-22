module Params where

data Params = Params {
    n :: Int,
    num_nodes :: Int,
    edge_mean :: Double,
    edge_sd :: Double,
    edge_threshold :: Double,
    agent_edge_mean :: Double,
    agent_edge_sd :: Double,
    agent_node_mean :: Double,
    agent_node_sd :: Double,
    agent_competence_sd :: Double,
    cor_age_competence :: Double
} deriving (Show, Eq)

