module Chapter4.Graphs where

import Data.Graph

-- input for building a task precedence graph
-- list of key, value, adjacency matrix
timeMachineGraph :: [ (String, String, [String]) ]
timeMachineGraph =
    [ ("wood", "wood", ["walls"])
    , ("plastic", "plastic", ["walls", "wheels"])
    , ("aluminum", "aluminum", ["wheels", "door"])
    , ("walls", "walls", ["done"])
    , ("door", "door", ["done"])
    , ("done", "done", [])
    ]

-- create a precendence graph from the input
timeMachinePrecedence :: (Graph, Vertex -> (String, String, [String]),  String -> Maybe Vertex)
timeMachinePrecedence = graphFromEdges timeMachineGraph

-- create plan by a topoligical sort of the precendence graph
createPlan :: (Graph, Vertex -> (b1, b2, c1), c2) -> [b1]
createPlan precGraph =
    let (g,v, _) = precGraph
    in map (\x -> let (k, _, _) = v x in k) $ topSort g

-- Time travel graphs 
-- Use buildG since we have integer identifieds. ARgs are min and max bounds, and a list of drected edges
timeMachineTravel :: Graph
timeMachineTravel = 
    buildG (103, 2013)
    [(1302, 1614), (1614, 1302), (1302, 2013), (2013, 1302), (1613, 2013), (2013, 1408),
    (1408, 1993), (1408, 917), (1993, 917), (917, 103), (103, 917)  ]

-- Can we travel from one time to another?
--      path timeMachineTravel t1 t2
-- What times can we travel to from t1?
--      reachable t1 timeMachineTravel
-- 