module Main where

import LearningEnvironment
import Params
import Agents
import Text.Printf (printf)
import Data.Array.Unboxed (bounds, (!))

main :: IO ()
main = do
    let params = Params {n = 100, num_nodes = 16, edge_mean = 0.5, edge_sigma = 0.2, edge_threshold = 0.4, agent_edge_mean = 0.0, agent_edge_sigma = 0.1}
    let seed = 12345
    let graph = initializeGraph seed params
    let agentWeights = createAgentWeights graph params seed

    -- Print initial adjacency matrix
    putStrLn "Initial Adjacency Matrix:"
    let adjMatrix = adjMat graph
    let ((_, _), (numRows, numCols)) = bounds adjMatrix
    let adjRows = [[adjMatrix ! (i, j) | j <- [1..numCols]] | i <- [1..numRows]]
    mapM_ (putStrLn . unwords . map (\x -> if x == 0 then " . " else printf "%.1f" x)) adjRows

    -- Print separator
    putStrLn "\nAdjacency Matrix for First Agent:"

    -- Print adjacency matrix for first agent
    let firstAgentSlice = extract2DSlice (weightMatrix agentWeights) 1
    print2DMatrix firstAgentSlice