module DirectDependencies(dp
                         ,idp
                         ) where

import CFG
import AG
import Control.Arrow((&&&),arr)
import qualified Data.Map  as Map
import qualified Data.List as List
import qualified Data.Set  as Set

-- | The dpBySemFunc is an ancillary function that calculates the
-- Dependencies for a sole production (ProdName).
dpBySemFunc :: ProdName
            -> SemFunc
            -> Map.Map AlphaAttr2 [AlphaAttr2]
dpBySemFunc pn (SF alpha _ beta) =
    foldl (Map.unionWith (++)) Map.empty [ Map.singleton  (fromAA2AA2 pn $ fromBetatoAlpha b) [(fromAA2AA2 pn a)]
                                               | a <- alpha
                                               , b <- beta
                                               , isPrAttr b]

-- | The dpSemFuncs is another ancillary function that help
-- to calculate the dependency graph.
dpSemFuncs :: ProdName -> SemFuncs -> (ProdName, Map.Map AlphaAttr2 [AlphaAttr2])
dpSemFuncs pn sfs = (pn, foldl (Map.unionWith (++)) Map.empty (map (dpBySemFunc pn) sfs))


-- | The 'dp' function calculate the dependency graph for all
-- productions defined on the semantic of an attribute grammar.
dp :: Semantic -> [(ProdName, Map.Map AlphaAttr2 [AlphaAttr2])]
dp m = map (\(k,v) -> dpSemFuncs k v) $ Map.toList m


-- | tc (Transitive Clousure of a graph. The graph is defined as a set
-- of edges (src-node, dst-node). It used a fixed-point function.
tc :: (Eq a) => [(a,a)] -> [(a,a)]
tc m = let itc mp = mp `List.union` [(a,d) | (a,b) <- mp, (c,d) <- mp, b == c]
           m' = itc m
       in if length m == length m'
          then m
          else tc m'

-- | The 'conv' function is a helper function which takes a graph
-- that is described by a map of arcs and convert it as an association list.
conv :: (Ord a) => Map.Map a [a] -> [(a,a)]
conv = concat.(map (\(x,xs) -> map (\x' -> (x,x')) xs)).(Map.toList)

-- | The 'conv' a association list with a graph and it flats that
-- structure into an asociation list which describe a graph.
conv' :: (Ord a) => [(b,Map.Map a [a])] -> [(a,a)]
conv' = foldl (\r (_,m) -> r `List.union` (conv m)) []

-- | The clausure transitive a graph represented by a Map
tcm :: (Ord a) => Map.Map a [a] -> Map.Map a a
tcm = (Map.fromList).tc.conv

-- | 
pairSet :: [a] -> [[a]]
pairSet m = [ [a,b]
                  | a <- m
                  , b <- m
            ]

iter_idp_p :: [(AlphaAttr2,AlphaAttr2)]
           -> [(AlphaAttr2,AlphaAttr2)]
           -> [[AlphaAttr2]]
           -> Prods
           -> ProdName
           -> [(AlphaAttr2,AlphaAttr2)]
iter_idp_p  idp_0 idp_plus opr_p prods p =
    [(pia, pib)
         | [pia@(AOut2 (AO2 _ i a )),pib@(AOut2 (AO2 _ i' b))] <- getIDPFromProdName p opr_p
         , i == i'
         , (pja@(AOut2 (AO2 p2 j a')),pjb@(AOut2 (AO2 p2' j' b'))) <- idp_plus
         , p2 == p2' && j == j'
         , getNonTerminalPos p i prods == getNonTerminalPos p2 j prods
    ]

idp' :: [(AlphaAttr2,AlphaAttr2)]
     -> [[AlphaAttr2]]
     -> Prods
     -> [ProdName]
     -> [(AlphaAttr2,AlphaAttr2)]
idp' idp_p_0 opr_p prods' prodns =
    let idp_plus_0 = tc idp_p_0
        idp_p_1    = map (iter_idp_p idp_p_0 idp_plus_0 opr_p prods') prodns
        idp_p_1'   = foldl (List.union) [] idp_p_1
    in if length idp_p_1' == length idp_p_0
       then idp_p_1'
       else idp' idp_p_1' opr_p prods' prodns

idp :: AG -> [(AlphaAttr2,AlphaAttr2)]
idp ag = let a_dp    = dp $ sem ag
             idp_p_0 = conv' a_dp
             opr     = (pairSet.(Set.elems)) $ occurPR ag
             prodns  = getProdNames (cfg ag)
             prods'  = prods.cfg $ ag
         in idp' idp_p_0 opr prods' prodns


getIDPFromProdName :: ProdName -> [[AlphaAttr2]] -> [[AlphaAttr2]]
getIDPFromProdName p opr_p =
    [[pia,pib] | [pia@(AOut2 (AO2 p' _ _)), pib@(AOut2 (AO2 p'' _ _))] <- opr_p
               , p == p' && p == p''
    ]




