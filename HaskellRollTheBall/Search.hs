{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where
import ProblemState
import Data.List as L
import Data.Maybe
import Data.Set as S

{-
Node type that includes:
    - current state
    - action that led to current state
    - current depth in state tree
    - all successors to current state
-}
data Node s a = Nod 
    { state :: s,
      action :: Maybe a,
      parent :: Maybe (Node s a),
      depth :: Int,
      children :: [(Node s a)]
      } deriving Eq

-- getters used by the checker
nodeState :: Node s a -> s
nodeState = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeDepth :: Node s a -> Int
nodeDepth = depth

nodeAction :: Node s a -> Maybe a
nodeAction = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children


-- function that creates the root for a state space
createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace s = Nod s Nothing Nothing 0 $ allChildren s Nothing 0 Nothing

{- helper that finds and returns all children for a node
    nState - current state
    acti - action for current state
    dpth - current depth
    pp - parent for current state
    Every child will have a parent node that is the parent without children
-}
allChildren :: (ProblemState s a, Eq s) => s -> Maybe a -> Int -> Maybe (Node s a) -> [Node s a]
allChildren nState act dpth pp = let suc = successors nState
                                     crtParent = Just (Nod nState act pp dpth [])
                                    in if L.null suc then [] else [Nod (snd x) (Just (fst x)) 
                                        crtParent (dpth+1) $ allChildren (snd x) (Just (fst x))
                                        (dpth+1) crtParent | x <- suc]
                               
-- starting point for my bfsHelper recursive function
bfs :: (ProblemState s a, Eq s, Ord s) => Node s a -> [([Node s a], [Node s a])]
bfs root = bfsHelper S.empty [root]

{-
    recursively build (newNodes, queue) stream by going through all unvisited children
    from every node
-}
bfsHelper :: (ProblemState s a, Eq s, Ord s) => Set s -> [Node s a] -> [([Node s a], [Node s a])]
bfsHelper _ [] = []
bfsHelper visited queue = (newChild, newQ) : (bfsHelper newVis newQ)
            where nod = head queue
                  newChild = L.filter (\x -> S.notMember (state x) visited) $ children nod
                  newVis = L.foldr S.insert visited (L.map state newChild)
                  newQ =  (tail queue) ++ newChild

{-
    we get bfs results from both start and finish then for we search for common elements
    in every start queue and finish new added nodes
    I use take function on a intersectBy to only evaluate until we find a common element
    then I extract the nodes coresponding to found value from both queues 
-}
bidirBFS :: (ProblemState s a, Eq s, Ord s) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS st fin = let fromStart = L.map snd $ bfs st
                      fromFin = L.map fst $ bfs fin
                      commonElem q1 q2 = take 1 (intersectBy (\el1 el2 -> state el1 == (state el2)) q1 q2)
                    in fromJust $ head $ L.filter (\x -> not $ isNothing x)
                                $ zipWith (compRes commonElem) fromStart fromFin

{- 
    helper function that take another function and 2 queues and
    returns the needed pair by extracting first node that appears in both queues
-}
compRes :: Eq s => ([Node s a] -> [Node s a] -> [Node s a]) 
                -> [Node s a] -> [Node s a] -> Maybe (Node s a, Node s a)
compRes func q1 q2 = if L.null foundElem then Nothing else Just (extractNode q1, extractNode q2)
            where foundElem = func q1 q2
                  extractNode q = head $ L.filter (\el -> state el == (state $ head foundElem)) q

{-
 use "iterate" to create infinite list with parent as function, then extract
 first depth+1(from depth to 0) and apply a foldl on this list to build
 (action, state) list from node list, while reversing it to have our starting
 node at the end
-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath (Nod s _ Nothing _ _) = [(Nothing,s)] 
extractPath node = L.foldl (\acc x-> (action x, state x) : acc) []
                        $ take ((depth node) + 1) $ iterate (fromJust.parent) node

{-
    create nodes then get intersection with bidirBFS - first half of the list is
    prepared, but for the second one we need to exclude the final state node,
    then to apply reverseAction on every node and reverse the list so the
    path is in the right order

-}
solve :: (ProblemState s a, Ord s)
      => s         -- initial state
      -> s          -- final state
      -> [(Maybe a, s)]   -- all pairs - path
solve start finish = let nodeSt = createStateSpace start
                         nodeFin = createStateSpace finish
                         comPair = bidirBFS nodeSt nodeFin
                         fromStart = extractPath (fst comPair)
                         tillEnd = tail $ extractPath $ snd comPair
                    in fromStart ++ (reverse (L.map (\x -> (Just (fst x), snd x)) 
                                    (L.map (\x -> reverseAction (fromJust (fst x), snd x)) tillEnd)))
