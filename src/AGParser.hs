module AGParser(parseIOAG
               ,scanAG
               ) where

import UU.Parsing
import UU.Scanner
import CFG
import AG
import qualified Data.Set as Set
import qualified Data.Map as Map

kwtxt  = [ "cfg","terminals","nonterminals","productions"
         ,"axiom", "t", "nt"
         ,"inherited", "synthesized", "local"
         ,"semantic"]
kwotxt = []
sctxt  = "{}();[],=:>< "
octxt  = ""

posTxt :: FilePath -> Pos
posTxt fn = Pos 0 0 fn

scanAG :: FilePath -> String -> [Token]
scanAG fn = scan kwtxt kwotxt sctxt octxt (posTxt fn)

pTerminal :: Parser Token String
pTerminal = pString

pTerminals :: Parser Token Terminals
pTerminals = Set.fromList <$> (pKey "terminals" *> pBracks_pCommas pTerminal)

pNonTerminal :: Parser Token String
pNonTerminal = pString

pNonTerminals :: Parser Token NonTerminals
pNonTerminals = Set.fromList <$> (pKey "nonterminals" *> pBracks_pCommas pNonTerminal)

pVTerminal :: Parser Token Vocabulary
pVTerminal = T <$> (pKey "t" *> pString)

pVNonTerminal :: Parser Token Vocabulary
pVNonTerminal = NT <$> (pKey "nt" *> pString)

pVocabulary :: Parser Token Vocabulary
pVocabulary =  pVTerminal
           <|> pVNonTerminal

pRHS :: Parser Token SeqProd
pRHS = pBracks_pCommas pVocabulary

pLHS :: Parser Token NonTerminal
pLHS = pNonTerminal

pProd :: Parser Token Prod
pProd = f <$> (pOParen *> pLHS <* pComma) <*> (pRHS <* pCParen)
        where f x y = (x,y)

pProdName :: Parser Token ProdName
pProdName = pString

pProdNamed :: Parser Token Prods
pProdNamed = f <$> pProdName <*> pProd
    where f x y = Map.singleton x y

pProds :: Parser Token Prods
pProds =  foldr (Map.union) Map.empty <$> pCurly_pSemics pProdNamed

pAxiom :: Parser Token NonTerminal
pAxiom = pKey "axiom" *> pNonTerminal

pCFG :: Parser Token CFG
pCFG = f <$> (pKey "cfg" *> pSpec '=' *> pTerminals) <*> pNonTerminals <*> pProds <*> pAxiom
       where f t nt ps a = CFG { terms = t
                               , nonterms = nt
                               , prods = ps
                               , axiom = a
                               }

pAttrName :: Parser Token AttrName
pAttrName = pString

pAttrNames :: Parser Token AttrNames
pAttrNames = Set.fromList <$> pCurly_pSemics pAttrName

pInhAttr :: Parser Token InhAttr
pInhAttr = pKey "inherited" *> pSpec '=' *> pAttrNames

pSynAttr :: Parser Token SynAttr
pSynAttr = pKey "synthesized" *> pSpec '=' *> pAttrNames

pType :: Parser Token TypeName
pType = pString

pAttrType :: Parser Token AttrType
pAttrType = f <$> pAttrName <*  pSpec ':' <*> pType
    where f x y = (x,y)

pAttrTypes :: Parser Token AttrTypes
pAttrTypes = pCurly_pSemics pAttrType

pInhAttrTypes :: Parser Token InhAttrTypes
pInhAttrTypes = pKey "inherited" *> pSpec '=' *> pAttrTypes

pSynAttrTypes :: Parser Token SynAttrTypes
pSynAttrTypes = pKey "synthesized" *> pSpec '=' *> pAttrTypes

pLocAttrTypes :: Parser Token LocAttrTypes
pLocAttrTypes = pKey "local" *> pSpec '=' *> pAttrTypes

pInhSynLocAttrTypes :: Parser Token InhSynLocAttrTypes
pInhSynLocAttrTypes = ISLAT <$> pInhAttrTypes <*> pSynAttrTypes <*> pLocAttrTypes

pInhSynLocAttrsTypes_X :: Parser Token InhSynLocAttrsTypes_X
pInhSynLocAttrsTypes_X = Map.singleton <$> pProdName <*> pInhSynLocAttrTypes

pProdAttrs :: Parser Token InhSynLocAttrsTypes_X
pProdAttrs = foldr (Map.union) Map.empty 
             <$> pCurly_pSemics pInhSynLocAttrsTypes_X

pAttrs :: Parser Token Attrs
pAttrs = Attrs <$> pInhAttr <*> pSynAttr <*> pProdAttrs

pInt :: Parser Token Int
pInt = read <$> pInteger

pAttrOccur :: Parser Token AttrOccur
pAttrOccur = AO <$> pInt <*> pAttrName

pLocAttr :: Parser Token String
pLocAttr = pAttrName

pAttrOccurB :: Parser Token AttrOccur
pAttrOccurB = pSpec '<' *> pAttrOccur <* pSpec '>'

pAlphaAttrOccur :: Parser Token AlphaAttr
pAlphaAttrOccur = AOut <$> pAttrOccurB

pAlphaAttrOLoc :: Parser Token AlphaAttr
pAlphaAttrOLoc = AOloc <$> pLocAttr

pAlphaAttr :: Parser Token AlphaAttrs 
pAlphaAttr = pParens_pCommas (pAlphaAttrOccur <|> pAlphaAttrOLoc)

pBetaAttrOccur :: Parser Token BetaAttr
pBetaAttrOccur = BOnt <$> pAttrOccurB

pBetaAttrOLoc :: Parser Token BetaAttr
pBetaAttrOLoc = BOloc <$> pLocAttr

pBetaAttrSynR :: Parser Token BetaAttr
pBetaAttrSynR =  BSynR <$> pBracks pVocabulary 

pBetaAttr :: Parser Token BetaAttrs
pBetaAttr = pList_gr (pBetaAttrOccur <|> pBetaAttrOLoc <|> pBetaAttrSynR)

pFunName :: Parser Token String
pFunName = pString

pSemFunc :: Parser Token SemFunc
pSemFunc = SF <$> pAlphaAttr <*> pFunName <*> pBetaAttr

pSemFuncs :: Parser Token SemFuncs
pSemFuncs = pBracks_pCommas pSemFunc

pProdSemFuncs :: Parser Token Semantic
pProdSemFuncs = Map.singleton <$> pProdName <*> pSemFuncs

pSemantic :: Parser Token Semantic
pSemantic = foldr (Map.union) Map.empty 
            <$> pCurly_pSemics pProdSemFuncs

pAG :: Parser Token AG
pAG = AG  <$> pCFG <*> pAttrs <*> pSemantic

parseIOAG :: [Token] -> IO AG
parseIOAG = parseIO pAG 