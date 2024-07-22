{-# LANGUAGE NamedFieldPuns #-}
module Agents where

import LearningEnvironment (Graph(..), Matrix2D)
import Params 
import Data.Array.Unboxed
import Data.List (foldl')
import Data.Random.Normal (mkNormals')
import Text.Printf (printf)

type Matrix3D = UArray (Int, Int, Int) Double

newtype AgentCompetence = AgentCompetence {
    competenceMatrix :: Matrix2D
} deriving (Show, Eq)

createAgentCompetence :: Params -> AgentCompetence
createAgentCompetence params =
    let rows = n params
        cols = num_nodes params
        mat_bounds = ((1, 1), (rows, cols))
        emptyMatrix = array mat_bounds [((i, j), 0.0) | i <- [1..rows], j <- [1..cols]]
    in AgentCompetence emptyMatrix

newtype AgentWeights = AgentWeights {
    weightMatrix :: Matrix3D
} deriving (Show, Eq)

createAgentWeights :: Graph -> Params -> Int -> AgentWeights
createAgentWeights Graph{adjMat} Params{n, num_nodes, agent_edge_mean, agent_edge_sigma} seed = 
    let zero3DMatrix = array ((1, 1, 1), (n, num_nodes, num_nodes)) [((i, j, k), 0.0) | i <- [1..n], j <- [1..num_nodes], k <- [1..num_nodes]]
        initialWeightsMatrix = foldl' (\acc ((i, j), w) -> accum (\_ x -> x) acc [((nIndex, i, j), w) | nIndex <- [1..n]]) zero3DMatrix (assocs adjMat)

        randomValues = drop num_nodes $ mkNormals' (agent_edge_mean, agent_edge_sigma) seed
        ((_, _, _), (_, r, c)) = bounds initialWeightsMatrix
        indexedRandomValues = zip [(i, j, k) | i <- [1..n], j <- [1..r], k <- [1..c]] randomValues

        updateValue :: Double -> Double -> Double
        updateValue val rand = let updated = max 0 (val + rand) in updated
        
        updatedEntries = [((i, j, k), updateValue (initialWeightsMatrix ! (i, j, k)) rand) | ((i, j, k), rand) <- indexedRandomValues, initialWeightsMatrix ! (i, j, k) /= 0]

        randomizedWeightsMatrix = initialWeightsMatrix // updatedEntries
    in AgentWeights randomizedWeightsMatrix

columnSums :: Matrix2D -> UArray Int Double
columnSums mat =
    let (_, (nRows, nCols)) = bounds mat
        initialSums = array (1, nCols) [(j, 0.0) | j <- [1..nCols]]
        sums = accum (+) initialSums [(j, mat ! (i, j)) | i <- [1..nRows], j <- [1..nCols]]
    in sums

{- todo: 
    - createAgentCompetence needs to be changed to incorporate random initial values
    - strategy: columnSums of the weight matrix gives the indegree of each node
    - nodes with higher indegree should have lower initial competence
    - we can use the inverse indegree to determine the average of the distribution from which the competence values are drawn (SD should be constant)
    - The problem is, we need to somehow ensure that the competence values are scaled appropriately
    - Either we can normalize the competence values into a predifined range, or we can scale the competence values by a predefined constant
    - Either idea sounds like a bad headache later on
-}

extract2DSlice :: Matrix3D -> Int -> Matrix2D
extract2DSlice matrix sliceIdx =
    let ((_, _, _), (_, numRows, numCols)) = bounds matrix
        sliceBounds = ((1, 1), (numRows, numCols))
        sliceElems = [((i, j), matrix ! (sliceIdx, i, j)) | i <- [1..numRows], j <- [1..numCols]]
    in array sliceBounds sliceElems

print2DMatrix :: Matrix2D -> IO ()
print2DMatrix matrix =
    let ((_, _), (numRows, numCols)) = bounds matrix
        rows = [[matrix ! (i, j) | j <- [1..numCols]] | i <- [1..numRows]]
    in mapM_ (putStrLn . unwords . map (\x -> if x == 0 then " . " else printf "%.1f" x)) rows

