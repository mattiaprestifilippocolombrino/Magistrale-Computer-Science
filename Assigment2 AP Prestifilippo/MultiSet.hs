module MultiSet
  ( MSet(..)   
  , empty      
  , add        
  , occs        
  , elems       
  , subeq       
  , union    
  , mapMSet    
  ) where

import Prelude hiding (foldr)
import qualified Prelude as P (foldr)
import Data.List (nub, sortOn)

-- Rappresentiamo un multiset come una lista di coppie (elemento a, molteplicità n).
data MSet a = MS [(a, Int)]
  deriving (Show) 

-- Funzioni di utilità interne

-- Funzione che verifica se un multiset è "ben formato".
wellFormed :: Eq a => MSet a -> Bool
wellFormed (MS pairs) = all (\(_, n) -> n > 0) pairs &&
  let vs = map fst pairs  -- Estrae tutti gli elementi dal multiset
  in length vs == length (nub vs)  -- Confronta la lunghezza con la lista senza duplicati

-- Condizioni per essere ben formato: 1.Nessuna molteplicità è minore o uguale a zero. 2. Nessun elemento compare più di una volta.
--all: verifica se tutti gli elementi di una lista soddisfano una certa condizione.
-- La lambda function prende una coppia MS e verifica se il secondo elemento n è maggiore di zero. 
--La variabile vs = map fst pairs, dove map applica ad ogni elemento della lista fstm, che estrae l'elemento di tipo a.
-- length vs == length (nub vs):  Calcola la lunghezza della lista e la confronta con la lunghezza della lista senza duplicati.


-- | Funzione che unisce due liste di coppie (elemento, molteplicità).
mergePairs :: Eq a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
mergePairs [] ys = ys 
mergePairs ((x, n):xs) ys =
  case lookup x ys of
    Nothing ->
      (x, n) : mergePairs xs ys
    Just m  ->
      (x, n + m) : mergePairs xs (filter ((x /=) . fst) ys)

-- Se un elemento appare in entrambe le liste, somma le molteplicità.
-- Se la prima lista è vuota, ritorna la seconda lista. Altrimenti:
-- Controlla che x sia presente in ys. Se non è presente, lo aggiunge in testa al risultante della chiamata ricorsiva e cpntinua a elaborare.
-- Altrimenti, lo aggiunge in testa alla chiamata ricorsiva, eliminando gli elementi uguali a x dalla seconda lista.
-- Altrimenti, 


-- | Funzione che rimuove eventuali coppie con molteplicità <= 0. Filtra la lista mantenendo le coppie con n>0.
cleanup :: Eq a => MSet a -> MSet a
cleanup (MS pairs) =
  MS $ filter (\(_, n) -> n > 0) pairs 


-- empty: Funzione che crea un multiset vuoto. Inizializza una lista MS vuota.
empty :: MSet a
empty = MS []


-- add: Funzione che aggiunge un elemento al multiset.
add :: Eq a => MSet a -> a -> MSet a
add (MS pairs) v =
  let newPairs = addEl pairs  -- Chiama la funzione ausiliaria 'go' per aggiornare le coppie
   in cleanup (MS newPairs)  -- Pulisce eventuali coppie con molteplicità <= 0
  where
    -- Funzione ausiliaria ricorsiva che aggiorna la lista di coppie
    addEl [] = [(v, 1)]  
    addEl ((x, n):xs)
      | x == v    = (x, n + 1) : xs  -- Se trova l'elemento, incrementa la molteplicità
      | otherwise = (x, n)     : addEl xs  -- Altrimenti, continua ricorsivamente

-- Applica alla lista la funzione addEl: Se la lista è vuota, aggiunge l'elemento con molteplicità 1.
-- Se trova l'elemento nella lista, incrementa la molteplicità. Altrimenti, continua con gli altri elementi della lista.


-- occs: Funzione che ritorna il numero di occorrenze di un elemento nel multiset.
-- Controlla v nella lista:Se trova l'elemento, ritorna la molteplicità. Altrimenti, ritorna 0.
occs :: Eq a => MSet a -> a -> Int
occs (MS pairs) v =
  case lookup v pairs of
    Just n  -> n  
    Nothing -> 0 



-- elems: Funzione che ritorna tutti gli elementi del multiset, ciascuno ripetuto secondo la sua molteplicità.
-- Ad esempio, MS [('a', 2), ('b', 3)] diventa ["a", "a", "b", "b", "b"].
elems :: MSet a -> [a]
elems (MS pairs) =
  concatMap (\(x, n) -> replicate n x) pairs  -- Per ogni coppia, replica l'elemento n volte e concatena

-- concatMap: Applica una funzione a ogni elemento della lista e concatena i risultati. Per ogni elemento, 
-- la lambda lo replica il numero delle sue molteplicita 


-- subeq: Funzione che verifica se un multiset sia un sottoinsieme di un altro. Restituisce True se tutti gli elementi di m1 compaiono in m2 con molteplicità >=.
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS xs) (MS ys) =
  all (\(v, n) ->
         case lookup v ys of
           Nothing  -> False  -- Se un elemento di m1 non è presente in m2, ritorna False
           Just n2  -> n <= n2  -- Altrimenti, verifica che la molteplicità in m2 sia >=
      ) xs

--Dati due mset, controlla che ogni coppia di xs sia presente in ysm con molteplicita n2 >n


-- union:Funzione che unisce due multiset, sommando le molteplicità di eventuali elementi comuni. Unisce le coppie con mercepairs e fa il clean del risultato con cleanup.
union :: Eq a => MSet a -> MSet a -> MSet a
union (MS xs) (MS ys) =
  cleanup (MS (mergePairs xs ys)) 


-- Istanza di Eq per MSet
-- Definiamo un'istanza di Eq per MSet, permettendo di confrontare due multiset per uguaglianza.
-- Vengono ordinate le due liste in base ad a, che deve essere ordinabile, e si effettua l'uguaglianza tra le due liste.
instance (Eq a, Ord a) => Eq (MSet a) where
  MS xs == MS ys =
    let sortedXs = sortOn fst xs  -- Ordina la prima lista per elemento
        sortedYs = sortOn fst ys  -- Ordina la seconda lista per elemento
    in sortedXs == sortedYs  -- Confronta le liste ordinate



-- Istanza di Foldable per MSet. Implementiamo foldr, che applica una funzione a ogni elemento del multiset.
instance Foldable MSet where
  foldr f z (MS pairs) =
    P.foldr (\(val, mult) acc -> P.foldr f acc (replicate mult val)) z pairs

-- Per ogni coppia (val, mult), replica 'val' 'mult' volte e applica foldr, accumulando il risultato acc. 
-- La lambda viene applicata a ogni coppia nella lista pairs, combinando i risultati in un unico accumulo finale z.




-- mapMSet: Funzione che applica una funzione (a -> b) a tutti gli elementi di un MSet.
-- Se due elementi diversi si mappano nello stesso valore b, le loro molteplicità vengono sommate. Ad esempio, mapMSet (\x -> x `mod` 2) (MS [(1,2),(3,3)]) = MS [(1,2),(1,3)] = MS [(1,5)]
mapMSet :: (Eq b) => (a -> b) -> MSet a -> MSet b
mapMSet f (MS pairs) =
  let newPairs = P.foldr applyF [] pairs  
   in cleanup (MS newPairs)  
  where
    applyF (x, n) acc =
      let y = f x 
      in case lookup y acc of
           Nothing -> (y, n) : acc 
           Just m  -> (y, n + m) : filter ((y /=) . fst) acc  -- Altrimenti, somma le molteplicità

-- Utilizza foldr per applicare 'applyF' a ogni coppia, per poi pulire il risultato.applyF applica la funzione f.
-- Se il risultato y della funzione applicata all'elemento non è presente, lo aggiunge con molteplicità n.
-- Altrimenti, lo aggiunge con molteplicità n siommato con le altre, eliminando gli elementi y dalla lista. 


-- Non possiamo definire un'istanza Functor, perchè richiederebbe fmap :: (a -> b) -> MSet a -> MSet b,
-- ma in un multiset, se due elementi diversi finiscono nello stesso valore b,
-- occorre unificare le loro molteplicità. Questo "collassa" la struttura in modo non coerente 
-- con la mappatura punto-a-punto richiesta dalle leggi Functor.

