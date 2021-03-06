module AG(AttrName
         ,AttrNames
         ,TypeName
         ,AttrType
         ,AttrTypes
         ,InhAttrTypes
         ,SynAttrTypes
         ,LocAttrTypes
         ,InhSynLocAttrTypes(..)
         ,InhSynLocAttrsTypes_X
         ,InhAttr
         ,SynAttr
         ,AttrOccur(..)
         ,AttrOccur2(..)
         ,AttrOccur3
         ,Attrs(..)
         ,AlphaAttr(..)
         ,AlphaAttr2(..)
         ,BetaAttr(..)
         ,AlphaAttrs
         ,BetaAttrs
         ,FuncName
         ,SemFunc(..)
         ,SemFuncs
         ,Semantic
         ,AG(..)
         ,fromBetatoAlpha
         ,fromAO2AO2
         ,fromAA2AA2
         ,nonTermOccur
         ,occurPR
         ,occurPRInhSynLoc_p
         ,occurPR_p
         ,occurNTOccs_p
         ,isPrAttr)
    where

import CFG
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.Error 

type AttrName     = String
type AttrNames    = Set.Set String
type TypeName     = String

type AttrType     = (AttrName,TypeName)
type AttrTypes    = [AttrType]

type InhAttrTypes = AttrTypes
type SynAttrTypes = AttrTypes
type LocAttrTypes = AttrTypes

data InhSynLocAttrTypes = ISLAT { inhAttrTypes :: InhAttrTypes
                                , synAttrTypes :: SynAttrTypes
                                , locAttrTypes :: LocAttrTypes
                                } deriving (Show)

type InhAttr      = AttrNames
type SynAttr      = AttrNames

type InhSynLocAttrsTypes_X = Map.Map NonTerminal (InhSynLocAttrTypes)

data AttrOccur    = AO Int AttrName
                    deriving (Show,Eq,Ord)

data AttrOccur2   = AO2 ProdName Int AttrName
                  deriving (Show,Eq,Ord)

type AttrOccur3 = AlphaAttr

data Attrs = Attrs { inhAttr :: InhAttr
                   , synAttr :: SynAttr
                   , inhSynLocAttrTypes_X :: InhSynLocAttrsTypes_X
                   } deriving (Show)

data AlphaAttr = AOut  AttrOccur
               | AOloc AttrName
                 deriving (Show,Eq,Ord)

data AlphaAttr2 = AOut2 AttrOccur2
                | AOloc2 ProdName AttrName
                deriving (Show,Eq,Ord)

data BetaAttr  = BOnt  AttrOccur
               | BOloc AttrName
               | BSynR Vocabulary
                 deriving (Show,Eq,Ord)

type AlphaAttrs = [AlphaAttr]
type BetaAttrs  = [BetaAttr]

type FuncName   = String

data SemFunc    = SF { alphaAttrs :: AlphaAttrs 
                     , funcName   :: FuncName 
                     , betaAttrs  :: BetaAttrs
                     }
                deriving (Show, Eq)

type SemFuncs = [SemFunc]

type Semantic = Map.Map ProdName SemFuncs

data AG = AG { cfg   :: CFG
             , attrs :: Attrs
             , sem   :: Semantic
             }

class AttrPr a where
    isPrAttr :: a -> Bool

instance AttrPr AlphaAttr where
    isPrAttr _ = True

instance AttrPr BetaAttr where
    isPrAttr (BSynR _) = False
    isPrAttr _         = True

fromBetatoAlpha :: BetaAttr -> AlphaAttr
fromBetatoAlpha (BOnt a)  = AOut a
fromBetatoAlpha (BOloc a) = AOloc a

fromAO2AO2 :: ProdName -> AttrOccur -> AttrOccur2
fromAO2AO2 pn (AO i n)  = AO2 pn i n

fromAA2AA2 :: ProdName -> AlphaAttr -> AlphaAttr2
fromAA2AA2 pn (AOut a)  = AOut2 $ fromAO2AO2 pn a
fromAA2AA2 pn (AOloc a) = AOloc2 pn a

nonTermOccur :: ProdName 
             -> Int 
             -> InhSynLocAttrTypes 
             -> (Set.Set AlphaAttr2,Set.Set AlphaAttr2,Set.Set AlphaAttr2)
nonTermOccur pn i islat = ( aaNT $ inhAttrTypes islat 
                          , aaNT $ synAttrTypes islat
                          , anT  $ locAttrTypes islat
                          ) 
                    where aaNT  = foldl (\s (a,_) -> Set.insert (AOut2 (AO2 pn i a)) s) Set.empty
                          anT   = foldl (\s (a,_) -> Set.insert (AOloc2 pn a) s) Set.empty


-- 
occurPR' :: ProdName 
         -> Prod 
         -> InhSynLocAttrsTypes_X 
         -> (Set.Set AlphaAttr2,Set.Set AlphaAttr2,Set.Set AlphaAttr2)
occurPR' pn (nt0,seq) m = 
    foldl f (Set.empty,Set.empty,Set.empty) $ 
              [(0,nt0)] ++ (zip (iterate (+1) 1) (filterNonTermSeq seq))
                  where f (inh,syn,loc) (n,ter) = 
                            case Map.lookup ter m of
                              Just attrs -> let (inh',syn',loc') 
                                                    = nonTermOccur pn n attrs
                                            in (inh `Set.union` inh',
                                                syn `Set.union` syn',
                                                loc `Set.union` loc')
                              Nothing    -> error $ "The ter: " ++ ter ++ " is not defined on:\n" ++ (show m)

occurPRInhSynLoc_p :: ProdName -> Prods -> InhSynLocAttrsTypes_X -> (Set.Set AlphaAttr2, Set.Set AlphaAttr2, Set.Set AlphaAttr2)
occurPRInhSynLoc_p pn prods m = case Map.lookup pn prods of
                                  Just prod -> occurPR' pn prod m 

occurPR_p :: InhSynLocAttrsTypes_X -> Prods -> ProdName  -> Set.Set AlphaAttr2
occurPR_p m prods pn = let (inh,syn,loc) = occurPRInhSynLoc_p pn prods m 
                        in inh `Set.union` syn `Set.union` loc

occurPR :: AG -> Set.Set AlphaAttr2
occurPR ag = let cfg'    = cfg ag
                 prods'  = prods cfg'
                 inhSynL = (inhSynLocAttrTypes_X.attrs) ag
                 prodsNs = getProdNames cfg'
             in foldr Set.union Set.empty $ 
                map (occurPR_p inhSynL prods') prodsNs 

occurNTOccs_p :: ProdName -> Prods -> InhSynLocAttrsTypes_X -> Set.Set AlphaAttr2
occurNTOccs_p pn prod m = let (inh,syn,_) = occurPRInhSynLoc_p pn prod m
                          in inh `Set.union` syn

-- occurNT :: ProdName -> Prod -> Attrs -> Set.Set AlphaAttr2 
-- occurNT pn prod (_,_,attrs) = occurInh pn prod attrs `Set.union` occurSyn pn prod attrs

-- occurPR_p :: ProdName -> AG -> Maybe (Set.Set AlphaAttr2)
-- occurPR_p pn ((_,_,prds,_),attrs,_) = do prd <- Map.lookup pn prds
--                                          return $ occurNT pn prd attrs