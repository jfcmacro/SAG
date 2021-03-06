module CFG (NonTerminal
           ,Terminal
           ,NonTerminals
           ,Terminals
           ,Vocabulary(..)
           ,SeqProd
           ,Prod
           ,ProdName
           ,Prods
           ,CFG(..)
           ,isCFGWellDefined
           ,getNonTerminalPos
           ,filterNonTermSeq
           ,getProdNames) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type NonTerminal  = String
type Terminal     = String
type NonTerminals = Set.Set NonTerminal
type Terminals    = Set.Set Terminal

data Vocabulary   = T Terminal
                  | NT NonTerminal
                    deriving (Show,Eq,Ord)

type SeqProd      = [Vocabulary]
type Prod         = (NonTerminal, SeqProd)

type ProdName     = String
type Prods        = Map.Map ProdName Prod

data CFG   = CFG { terms :: Terminals
                 , nonterms :: NonTerminals
                 , prods :: Prods
                 , axiom :: NonTerminal
                 }

isCFGWellDefined :: CFG -> Either String Bool
isCFGWellDefined cfg = isValidAxiom (axiom cfg) (nonterms cfg) 
                       `bind` 
                       isProdsWellDefined (nonterms cfg) (terms cfg) (prods cfg)

isValidAxiom :: NonTerminal -> NonTerminals -> Either String Bool
isValidAxiom nt nts = if nt `Set.member` nts
                      then Right True
                      else Left  "Axiom is not defined into nonterminals"

isVocabularyWellDefined :: NonTerminals -> Terminals -> Vocabulary -> Either String Bool
isVocabularyWellDefined _  ts (T t)    = if Set.member t   ts
                                         then Right True
                                         else Left  $ (show t) ++ " is not member of terminals"
isVocabularyWellDefined nt _  (NT nt') = if Set.member nt' nt
                                         then Right True
                                         else Left $ (show nt) ++ " is not member of nonterminals"

bind :: Either String Bool -> Either String Bool -> Either String Bool
bind (Left s) (Left s')   = Left (s ++ s')
bind (Left s) _           = Left s
bind (Right b) (Left s)   = Left s
bind (Right b) (Right b') = Right (b && b')

isSeqProdWellDefined :: NonTerminals -> Terminals -> SeqProd -> Either String Bool
isSeqProdWellDefined nt ts = foldl (\b v -> b `bind` isVocabularyWellDefined nt ts v) (Right True)

isProdWellDefined :: NonTerminals -> Terminals -> Prod -> Either String Bool 
isProdWellDefined nt ts (nt',seq) = if nt' `Set.member` nt 
                                    then Right True `bind` isSeqProdWellDefined nt ts seq
                                    else Left $ (show nt')++" is not member of nonterminals"

isProdsWellDefined :: NonTerminals -> Terminals -> Prods -> Either String Bool
isProdsWellDefined nt ts = Map.fold f (Right True) 
                           where f p b = b `bind` isProdWellDefined nt ts p

getNPProd :: ProdName -> Prods -> Maybe Int
getNPProd pn m = do (_,ls) <- Map.lookup pn m
                    return (length ls + 1)

getNonTerminalPos :: ProdName -> Int -> Prods -> Maybe Terminal
getNonTerminalPos pn i m = 
    do (n,ls) <- Map.lookup pn m 
       if i == 0
        then return n
        else case (ls !! (i - 1)) of
               T t -> return t
               _   -> Nothing

isVocTerm :: Vocabulary -> Bool
isVocTerm (T _) = True
isVocTerm _     = False

isVocNonTerm :: Vocabulary -> Bool
isVocNonTerm = not.isVocTerm

filterNonTermSeq :: SeqProd -> [NonTerminal]
filterNonTermSeq = map(\(NT nt) -> nt).filter isVocNonTerm  


getProdNames :: CFG -> [ProdName]
getProdNames cfg = Map.keys $ prods cfg