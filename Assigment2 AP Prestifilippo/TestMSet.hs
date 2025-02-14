module TestMSet where

import Prelude 
import System.IO   (readFile, writeFile)  
import Data.Char   (toLower)            
import Data.List   (sort, nub)        


import MultiSet
  ( MSet(..)    
  , empty       
  , add         
  , union       
  , elems      
  , subeq       
  , mapMSet    
  )


-- readMSet: Funzione che Legge un file di testo e costruisce un multiset di "ciao" (sort . map toLower)
-- Legge il contenuto del file, divide il contenuto in una lista di parole e Costruisce e ritorna il multiset utilizzando 'buildCiaoMSet'
 
readMSet :: FilePath -> IO (MSet String)
readMSet fileName = do
  content <- readFile fileName  
  let ws = words content         
  return (buildCiaoMSet ws)  



-- buildCiaoMSet: Costruisce un multiset di stringhe "ciao" partendo da una lista di parole.
-- Se la lista vuota produce un multiset vuoto. Altrimenti applica 'ciaoOf' alla parola w
-- e aggiunge il risultato al multiset creato ricorsivamente dalla funzione.
-- La funzione è ricorsiva: per ogni parola nella lista, 
buildCiaoMSet :: [String] -> MSet String
buildCiaoMSet [] = empty  
buildCiaoMSet (w:ws) =
  add (buildCiaoMSet ws) (ciaoOf w)

-- ciaoOf: Funzione che trasforma una parola in minuscolo e ordina i suoi caratteri.
ciaoOf :: String -> String
ciaoOf w = sort (map toLower w)



-- writeMSet:scrive ogni coppia (elemento, molteplicità) del multiset su una riga del file.
--  Converte ogni coppia in una stringa formattata. Scrive tutte le stringhe nel file specificato, una per riga.
writeMSet :: Show a => MSet a -> FilePath -> IO ()
writeMSet (MS pairs) outF = do
  let ls = map (\(x,n) -> show x ++ " - " ++ show n) pairs 
  writeFile outF (unlines ls) 


-- sameElements: Funzione che verifica se due multisets contengono gli stessi elementi, ignorando le molteplicità.
-- Estrae gli elementi dai due multiset, e utilizza nub per rimuovere i duplicati e sort per ordinare.
-- Confronta infine le liste di elementi unici e ordinati
sameElements :: (Ord a) => MSet a -> MSet a -> Bool
sameElements (MS as) (MS bs) =
  let keysA = map fst as         
      keysB = map fst bs  
  in sort (nub keysA) == sort (nub keysB)  


--main
--Carica m1 da "anagram.txt", carica m2 da "anagram_s1.txt", carica m3 da "anagram_s2.txt" e carica m4 da "margana2.txt".
-- Verifica che m1 e m4 non sono uguali, ma contengono gli stessi tipi di elementi.
-- Verifica che m1 è uguale all'unione di m2 e m3. Scrive m1 su "anag-out.txt" e m4 su "gana-out.txt"

main :: IO ()
main = do
  -- Caricamento dei file in 4 multisets diversi.
  m1 <- readMSet "aux_files/anagram.txt"       
  m2 <- readMSet "aux_files/anagram_s1.txt"    
  m3 <- readMSet "aux_files/anagram_s2.txt"    
  m4 <- readMSet "aux_files/margana2.txt"     

  checkFirst m1 m4
  checkSecond m1 m2 m3

  -- Scrive i risultati nei file di output.
  writeMSet m1 "aux_files/anag-out.txt"    
  writeMSet m4 "aux_files/gana-out.txt" 

  putStrLn "Scrittura completata." 



-- checkFirst: Funzione che verifica se due multisets non sono uguali ma contengono gli stessi tipi di elementi.
checkFirst :: (Eq a, Ord a, Show a) => MSet a -> MSet a -> IO ()
checkFirst m1 m4 =
  if (m1 /= m4) && sameElements m1 m4
    then putStrLn "OK: m1 e m4 non sono uguali, ma contengono gli stessi elementi."
    else putStrLn "ATTENZIONE: m1 e m4 non soddisfano la condizione!"


-- checkSecond: Funzione che verifica se un multiset è uguale all'unione di due altri multisets.
checkSecond :: (Eq a, Ord a) => MSet a -> MSet a -> MSet a -> IO ()
checkSecond m1 m2 m3 =
  if m1 == union m2 m3
    then putStrLn "OK: m1 è uguale all'unione di m2 e m3."
    else putStrLn "ATTENZIONE: m1 NON corrisponde all'unione di m2 e m3."
