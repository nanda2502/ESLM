module LearningEnvironment where

import Data.Random.Normal (mkNormals')
import Data.Maybe (fromMaybe)
import Params 
import Data.Array.Unboxed

createEdges::Params -> [(Int,Int)]
createEdges params = [(i, j) | i <- [1..num_nodes params], j <- [(i+1)..num_nodes params]]

assignWeights :: [(Int, Int)] -> Params -> Int -> [(Int, Int, Double)]
assignWeights edgeList params seed = 
    let weights = take (length edgeList) (mkNormals' (edge_mean params, edge_sigma params) seed) 
    in zipWith (\(i,j) w -> (i,j,w)) edgeList weights

pruneEdges :: [(Int, Int, Double)] -> Params -> [(Int, Int, Double)]
pruneEdges edgeList params = filter (\(_,_,w) -> w >= edge_threshold params) edgeList

createParents :: [(Int, Int, Double)] -> Params -> [(Int, [Int])]
createParents edgeList params = 
    let allNodes = [1..num_nodes params]
    in [(node, findParents node edgeList) | node <- allNodes]
    where findParents node e = [i | (i,j,_) <- e, j == node]

type Matrix2D = UArray (Int, Int) Double

data Graph = Graph {
    adjMat :: Matrix2D,
    parents :: [(Int, [Int])]
} deriving (Show)

createGraph :: [(Int, Int, Double)] -> Params -> Graph
createGraph prunedEdges params = Graph {
    adjMat = createAdjMat prunedEdges params,
    parents = createParents prunedEdges params
} 

initializeGraph :: Int -> Params -> Graph
initializeGraph seed params = 
    let initial_edges = createEdges params 
        weightedEdges = assignWeights initial_edges params seed
        prunedEdges = pruneEdges weightedEdges params
    in createGraph prunedEdges params

getParents :: Graph -> [Int] -> [[Int]]
getParents (Graph _ parentList) = map (\node -> fromMaybe [] (lookup node parentList))

createAdjMat :: [(Int, Int, Double)] -> Params -> Matrix2D
createAdjMat edges params = 
    let size = num_nodes params
        mat_bounds = ((1, 1), (size, size))
        zeroMatrix = array mat_bounds [((i, j), 0.0) | i <- [1..size], j <- [1..size]]
        matrix = accum (\_ w -> w) zeroMatrix [((i, j), w) | (i, j, w) <- edges]
    in matrix