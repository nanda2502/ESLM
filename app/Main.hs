module Main where

import LearningEnvironment
import Params
import Agents
import Utils

main :: IO ()
main = do
    let params = Params {
        n = 100,
        num_nodes = 16,
        edge_mean = 0.5,
        edge_sd = 0.2,
        edge_threshold = 0.4,
        agent_edge_mean = 0.0,
        agent_edge_sd = 0.1,
        agent_node_mean = 0.0,
        agent_node_sd = 0.1,
        agent_competence_sd = 0.1
    }
    let seed = 12345
    let graph = initializeGraph seed params
    let agentData = initializeAgents graph params seed

    putStrLn "Initial Adjacency Matrix:"
    print2DMatrix (adjMat graph)
    
    putStrLn "\nEdge Weights for First Agent:"
    let firstAgentSlice = extract2DSlice (edge_weights agentData) 1
    print2DMatrix firstAgentSlice

    putStrLn "\nNode Weights:"
    print2DMatrix (node_weights agentData)

    putStrLn "\nCompetence:"
    print2DMatrix (competence agentData)




