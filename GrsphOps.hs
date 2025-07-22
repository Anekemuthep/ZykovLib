import Data.Set (Set)
import qualified Data.Set as Set

data Graph = Graph { vertices :: Set Int, edges :: Set (Set Int) }

instance Show Graph where
  show (Graph vs es) = "Vertices: " ++ show (Set.toList vs) ++ "\nEdges: " ++ show (Set.toList es)

-- Function to compute the union of two graphs
unionGraphs :: Graph -> Graph -> Graph
unionGraphs (Graph vs1 es1) (Graph vs2 es2) = Graph (Set.union vs1 vs2) (Set.union es1 es2)

-- Example usage
g1 = Graph (Set.fromList [1,2,3]) (Set.fromList [Set.fromList [1,2], Set.fromList [1,3], Set.fromList [2,3]])
g2 = Graph (Set.fromList [4,5]) (Set.fromList [Set.fromList [4,5]])

g3 = unionGraphs g1 g2

graphToSets :: Ord a => ([a], [[a]]) -> (Set a, Set (Set a))
graphToSets (vs, es) = (Set.fromList vs, Set.fromList (map Set.fromList es))
