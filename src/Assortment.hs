module Assortment where 

import Data.Array.Unboxed
import Params
import Utils (Matrix2D, Matrix3D)
import Agents (AgentData(..))


createAssortment :: AgentData -> Params -> [UArray Int Int]
createAssortment AgentData{ages} Params{n, m} = 
    let sortedAges = sort ages
        groupSize = n `div` m
        groups = [take groupSize (drop (i * groupSize) sortedAges) | i <- [0..(m-1)]]
    in map (\grp -> listArray (0, length grp - 1) grp :: UArray Int Int) groups