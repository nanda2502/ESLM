{-# LANGUAGE NamedFieldPuns #-}
module Agents where

import LearningEnvironment (Graph(..))
import Params 
import Data.Array.Unboxed
import Data.List (foldl')
import Data.Random.Normal (mkNormals')
import System.Random (mkStdGen, randomRs)
import Utils (Matrix2D, Matrix3D)

createAgentCompetence :: Graph -> Params -> Int -> UArray (Int, Int) Double
createAgentCompetence Graph{adjMat} Params{n, num_nodes, agent_competence_sd} seed =
    let rows = n
        cols = num_nodes
        mat_bounds = ((1, 1), (rows, cols))
        indegreeSums = elems $ colSums adjMat
        maxSum = maximum indegreeSums
        minSum = minimum indegreeSums
        normalizedIndegree = map (\x -> (x - minSum) / (maxSum - minSum)) indegreeSums
        meanCompetence = map (1 -) normalizedIndegree
        randomValues = mkNormals' (0, agent_competence_sd) seed
        competenceValues = [max 0.0 (meanCompetence !! (j - 1) + rand) | ((_, j), rand) <- zip (range mat_bounds) randomValues]
        competenceMatrix = array mat_bounds (zip (range mat_bounds) competenceValues) :: UArray (Int, Int) Double
    in competenceMatrix

data AgentWeights = AgentWeights {
    edgeMatrix :: Matrix3D,
    nodeMatrix :: Matrix2D
} deriving (Show, Eq)

createAgentWeights :: Graph -> Params -> Int -> AgentWeights
createAgentWeights Graph{adjMat} Params{n, num_nodes, agent_edge_mean, agent_edge_sd, agent_node_mean, agent_node_sd} seed = 
    let zero3DMatrix = array ((1, 1, 1), (n, num_nodes, num_nodes)) [((i, j, k), 0.0) 
            | i <- [1..n], j <- [1..num_nodes], k <- [1..num_nodes]]
        
        initialWeightsMatrix = foldl' (\acc ((i, j), w) -> accum (\_ x -> x) acc [((nIndex, i, j), w) 
            | nIndex <- [1..n]]) zero3DMatrix (assocs adjMat)

        randomValuesEdge = mkNormals' (agent_edge_mean, agent_edge_sd) seed
        ((_, _, _), (_, r, c)) = bounds initialWeightsMatrix
        indexedRandomValuesEdge = zip [(i, j, k) | i <- [1..n], j <- [1..r], k <- [1..c]] randomValuesEdge

        updateValue :: Double -> Double -> Double
        updateValue val rand = max 0 (val + rand)
        
        updatedEntries = [((i, j, k), updateValue (initialWeightsMatrix ! (i, j, k)) rand) 
            | ((i, j, k), rand) <- indexedRandomValuesEdge, initialWeightsMatrix ! (i, j, k) /= 0]

        randomizedWeightsMatrix = initialWeightsMatrix // updatedEntries
        
        randomValuesNode = mkNormals' (agent_node_mean, agent_node_sd) (seed + 1)
        nodeMatrix = array ((1, 1), (n, num_nodes)) [((i, j), rand) 
            | ((i, j), rand) <- zip [(i, j) | i <- [1..n], j <- [1..num_nodes]] randomValuesNode]
    in AgentWeights randomizedWeightsMatrix nodeMatrix

colSums :: Matrix2D -> UArray Int Double
colSums mat = let ((_, minCol), (_, maxCol)) = bounds mat in accumArray (+) 0 (minCol, maxCol) [(j, mat ! (i, j)) | (i, j) <- indices mat]

rowSums :: Matrix2D -> UArray Int Double
rowSums mat = let ((minRow, _), (maxRow, _)) = bounds mat in accumArray (+) 0 (minRow, maxRow) [(i, mat ! (i, j)) | (i, j) <- indices mat]

{- age is partly determined by the total competence of the agent and partly
 randomly with a uniform distribution, and is scaled to be between 0 and 100-}
createAges :: Matrix2D -> Params -> Int -> UArray Int Double
createAges competences Params{cor_age_competence} seed =
    let competencesList = rowSums competences :: UArray Int Double
        maxCompetence = maximum (elems competencesList)
        minCompetence = minimum (elems competencesList)
        normalizedCompetences = amap (\c -> (c - minCompetence) / (maxCompetence - minCompetence)) competencesList
        gen = mkStdGen seed
        randomAges = randomRs (0, 1) gen :: [Double]
        ages = listArray (bounds normalizedCompetences) 
            [fromIntegral (round (100 * (cor_age_competence * c + (1 - cor_age_competence) * r))) 
            | (c, r) <- zip (elems normalizedCompetences) randomAges] :: UArray Int Double
    in ages

data AgentData = AgentData {
    edge_weights :: Matrix3D,
    node_weights :: Matrix2D,
    competences :: Matrix2D,
    ages :: UArray Int Double
} deriving (Show, Eq)

initializeAgents :: Graph -> Params -> Int -> AgentData
initializeAgents graph params seed =
    let agentWeights = createAgentWeights graph params seed
        agentCompetence = createAgentCompetence graph params seed
        ages = createAges agentCompetence params seed
    in AgentData (edgeMatrix agentWeights) (nodeMatrix agentWeights) agentCompetence ages

