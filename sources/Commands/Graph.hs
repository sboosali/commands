{-# LANGUAGE LambdaCase #-}
module Commands.Graph where
import Data.Graph
import Data.Maybe


type Adjacency k n = (n, k, [k])

{- | the maximal cycles of a directed graph (represented as an adjacency list)

wraps 'stronglyConnComp'

>>> :{
let graph = [ ("non recursive",        "N", [])
            , ("self recursive",       "S", ["S"])
            , ("mutually recursive A", "A", ["B"])
            , ("mutually recursive B", "B", ["A","C"])
            , ("mutually recursive C", "C", ["A","S","N"])
            ]
:}

>>> cycles graph
[["self recursive"],["mutually recursive A","mutually recursive B","mutually recursive C"]]

properties:

* the output @[[n]]@ is disjoint i.e. the cycles are maximal
* each output element @n@ comes from the input @Graph n e@ (but not the converse e.g. the output can be empty)
* when input an acyclic graph, the empty list is output (a singleton means the node has an edge to itself i.e. self-recursion) (in particular, the empty graph, lists, trees, DAGs)
* when input a complete graph, the singleton list of (the list of) vertices is output

TODO verify:
\as -> Set.fromList (map fst $ as) == Set.fromList (flattenSCCs . stronglyConnComp $ as)
i.e. it preserves the exact input nodes. which means that the partial 'find' can be safely assumed total.


-}
cycles :: Ord k => [Adjacency k n] -> [[n]]
cycles = sccs2cycles . stronglyConnComp


sccs2cycles :: [SCC n] -> [[n]]
sccs2cycles = mapMaybe $ \case
 AcyclicSCC _ -> Nothing
 CyclicSCC ns -> Just ns

-- -- |
-- --
-- -- guarantees that the node of the first 'Adjacency' is in the first 'SCC':
-- --
-- -- something like prop> let (c:|_) = stronglyConnCompNonEmpty (a:|as) in a == c
-- --
-- stronglyConnCompNonEmpty :: Ord k => NonEmpty (Adjacency k n) -> NonEmpty (SCC n)
-- -- stronglyConnCompNonEmpty :: Ord k => NonEmpty (Adjacency k n) -> (n, SCC n, [SCC n])
-- stronglyConnCompNonEmpty (a:|as) = c:|cs
--  where
--  (c,cs) = partition $ stronglyConnComp (a:as)
