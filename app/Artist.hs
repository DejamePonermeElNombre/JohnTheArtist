{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Artist where

import UdGraphic
import Test.QuickCheck
import Debug.Trace



instance Show Comanda where
    show (Avança n) = "Avança " ++ show n
    show (Gira n) = "Gira " ++ show n
    show Para = "Para"
    show (CanviaColor c) = "CanviaColor " ++ show c
    show (Branca c) = "Brancament " ++ show c
    show (c1 :#: c2) = show c1 ++ " :#: " ++ show c2


-- Problema 1

-- DESCRIPCIÓ: passar d'una comanda (o conjunt de comandes separades per :#: ) en una llista de comandes separades per comes [c1,c2,...]
separa :: Comanda -> [Comanda]
separa Para = [] -- Si para, retorn []
separa (c1 :#: c2) = separa c1 ++ separa c2 -- Si comanda composta, separa les dues comandes i ajunta les llistes
separa c = [c] -- Si no és comanda composta, retorna la comanda en una llista


-- Problema 2

-- DESCRIPCIÓ: passar d'una llista de comandes [c1,c2,...] en una comanda (o conjunt de comandes separades per :#: )
ajunta :: [Comanda] -> Comanda
ajunta [] = Para   -- Si la llista és buida, retorna Para
ajunta [c] = c -- Si la llista té un element, retorna l'element
ajunta (c1:c2:cs) = c1 :#: ajunta (c2:cs) -- Si la llista té més d'un element, ajunta els dos primers i crida recursivament amb la resta


-- Problema 3

-- DESCRIPCIÓ: comprova si dues comandes són equivalents
prop_equivalent :: Comanda -> Comanda -> Bool
prop_equivalent c1 c2 = separa c1 == separa c2 -- Si les llistes de comandes són iguals, són equivalents
instance Eq Comanda where -- Per poder comparar dues comandes (==)
  Avança n1 == Avança n2 = n1 == n2 -- Si són Avança, compara els nombres
  Gira n1 == Gira n2 = n1 == n2 -- Si són Gira, compara els nombres
  Para == Para = True -- Si són Para, són iguals
  CanviaColor c1 == CanviaColor c2 = c1 == c2  -- Si són CanviaColor, compara els colors
  Branca c1 == Branca c2 = c1 == c2 -- Si són Branca, compara les comandes
  (c11 :#: c12) == (c21 :#: c22) = c11 == c21 && c12 == c22 -- Si són comandes compostes, compara les comandes que les formen (recursivament)
  _ == _ = False -- Si no són del mateix tipus, no són iguals

-- DESCRIPCIÓ: Comprova que la comanda retornada per separa+ajunta és equivalent a la comanda original
prop_separa_ajunta :: Comanda -> Bool
prop_separa_ajunta c = prop_equivalent(ajunta(separa c)) c

-- DESCRIPCIÓ: Comprova que la llista retornada per separa no conté cap Para ni comanda composta
prop_separa :: Comanda -> Bool
prop_separa c = all ni_para_ni_compost (separa c) -- Comprova que totes les comandes de la llista compleixen la propietat niParaNiCompost
  where
    ni_para_ni_compost Para      = False -- Si és Para, retorna False
    ni_para_ni_compost (_ :#: _) = False -- Si és comanda composta, retorna False
    ni_para_ni_compost _         = True -- Si no és Para ni comanda composta, retorna True


-- Problema 4

-- DESCRIPCIÓ: copia una comanda n vegades
copia :: Int -> Comanda -> Comanda
copia 1 c = c -- Si n = 1, retorna la comanda
copia n c = c :#: copia (n - 1) c -- Si n > 1, retorna la comanda i crida recursivament amb n-1


-- Problema 5

-- DESCRIPCIÓ: retorna la comanda per dibuixar un pentàgon regular amb costats de mida distancia 
pentagon :: Distancia -> Comanda
pentagon distancia = copia 5 (Avança distancia :#: Gira 72) 


-- Problema 6

-- DESCRIPCIÓ: retorna la comanda per dibuixar un polígon regular de n costats de distància d
poligon :: Distancia -> Int -> Angle -> Comanda
poligon distancia nCostats angle = copia nCostats (Avança distancia :#: Gira angle) -- Crida a copia per copiar nCostats vegades la comanda (Avança distancia :#: Gira angle)

-- DESCRIPCIÓ: comprova que la comanda per dibuixar un polígon regular de n costats (que serà un pentàgon) de la distància entrada és equivalent a la comanda per dibuixar un pentàgon regular de distància d
prop_poligon_pentagon :: Distancia -> Bool
prop_poligon_pentagon distancia = prop_equivalent (poligon distancia 5 72) (pentagon distancia) 


-- Problema 7

-- DESCRIPCIÓ: dibuixa una espiral amb nSegm voltes, cada una de les quals té costat de longitud costat i està separat per un angle. La longitud del costat augmenta en pas en cada volta
espiral :: Distancia -> Int -> Distancia -> Angle -> Comanda
espiral costat nSegm pas angle = pas_espiral costat nSegm
  where
  pas_espiral costat nSegm | costat <= 0 || nSegm == 0 = Para -- Si la longitud del costat és negativa o nSegm = 0, retorna Para
         | otherwise = Avança costat :#: Gira angle :#: pas_espiral (costat+pas) (nSegm-1)-- Si la longitud del costat és positiva i nSegm > 0, retorna Avança costat :#: Gira angle :#: pas_espiral (costat+pas) (nSegm-1)


-- Problema 9

-- DESCRIPCIÓ: retorna una comanda que dibuixa el mateix que la rebuda, però sense Avances consecutius, Gires concecutius; ni Pares, ni Avança 0, ni Gira 0
optimitza :: Comanda -> Comanda
-- Primer separem, filtrem per avança, combinem, filtrem per fira, combinem i ajuntem
optimitza = ajunta . combinar . filter (/= Gira 0) . combinar . filter (/= Avança 0) . separa
    where
      combinar [] = []
      combinar (Gira x : Gira y : xs) = combinar (Gira (x+y) : xs) -- Si hi ha dues comandes Gira seguides, les combina
      combinar (Avança x : Avança y : xs) = combinar (Avança (x+y) : xs) -- Si hi ha dues comandes Avança seguides, les combina
      combinar (x:xs) = x : combinar xs -- Si no hi ha dues comandes seguides, crida recursivament amb la resta de la llista


-- Problema 10

-- DESCRIPCIÓ: retorna la comanda per dibuixar un triangle de costat de mida lvl
triangle :: Int -> Comanda
triangle lvl  =  pos :#: f lvl
  where
    f 0  = Avança 10 -- Si f = 0, retorna Avança 10
    f lvl  = f (lvl-1) :#: pos :#: f (lvl-1) :#: neg :#: f (lvl-1) :#: neg :#: f (lvl-1) :#: pos :#: f (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    neg  = Gira (-90) -- Representa signe '-' de la gramàtica
    pos  = Gira 90    -- Representa signe '+' de la gramàtica


-- Problema 11

-- DESCRIPCIÓ: retorna la comanda per dibuixar una fulla de costat de mida lvl
fulla :: Int -> Comanda
fulla lvl  =  f lvl -- lvl (level o nivell) serà el nombre de vegades que es farà recursivitat de la gramàtica
  where
    f 0 = CanviaColor vermell :#: Avança 10 -- Si f = 0, retorna Avança 10
    f lvl = g (lvl-1) :#: Branca (neg :#: f (lvl-1)) :#: Branca (pos :#: f (lvl-1)) :#: Branca (g (lvl-1) :#: f (lvl-1)) -- Va fent recursivitat amb les crides fins que valgui 0
    g 0 = CanviaColor blau :#: Avança 10 -- Si g = 0, retorna Avança 10
    g lvl = g (lvl-1) :#: g (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    neg = Gira (-45)  -- Representa signe '-' de la gramàtica
    pos = Gira 45     -- Representa signe '+' de la gramàtica 


-- Problema 12

-- DESCRIPCIÓ: retorna la comanda per dibuixar una fulla de costat de mida lvl
hilbert :: Int -> Comanda
hilbert lvl = l lvl
  where
    l 0 = Para -- Si lvl = 0, retorna Para
    l lvl = pos :#: r (lvl-1) :#: f :#: neg :#: l (lvl-1) :#: f :#: l (lvl-1) :#: neg :#: f :#: r (lvl-1) :#: pos -- Va fent recursivitat amb les crides fins que valgui 0
    r 0 = Para -- Si lvl = 0, retorna Para
    r lvl = neg :#: l (lvl-1) :#: f :#: pos :#: r (lvl-1) :#: f :#: r (lvl-1) :#: pos :#: f :#: l (lvl-1) :#: neg -- Va fent recursivitat amb les crides fins que valgui 0
    f = Avança 10 -- Si f = 0, retorna Avança 10
    neg = Gira (-90)  -- Representa signe '-' de la gramàtica
    pos = Gira 90     -- Representa signe '+' de la gramàtica


-- Problema 13

-- DESCRIPCIÓ: retorna la comanda per dibuixar una fletxa de amb la gramàtica reescrita lvl vegades i les lletres substituides per les comandes corresponents
fletxa :: Int -> Comanda
fletxa lvl = f lvl
  where
    f 0 = CanviaColor blau :#: Avança 10 -- Si lvl = 0, retorna Avança 10
    f lvl = g (lvl-1) :#: neg :#: f (lvl-1) :#: neg :#: g (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    g 0 = CanviaColor vermell :#: Avança 10 -- Si lvl = 0, retorna Avança 10
    g lvl = f (lvl-1) :#: pos :#: g (lvl-1) :#: pos :#: f (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    neg = Gira 60 -- Representa signe '-' de la gramàtica
    pos = Gira(-60) -- Representa signe '+' de la gramàtica


-- Problema 14
 
-- DESCRIPCIÓ: retorna la comanda per dibuixar un arbre de amb la gramàtica reescrita lvl vegades i les lletres substituides per les comandes corresponents
branca :: Int -> Comanda
branca lvl = g lvl
  where
    g 0 = CanviaColor verd :#: Avança 10 -- Si lvl = 0, retorna Avança 10
    g lvl = f (lvl-1) :#: neg :#: Branca (Branca (g (lvl-1)) :#: pos :#: g (lvl-1)) :#: pos :#: f (lvl-1) :#: Branca (pos :#: f (lvl-1) :#: g (lvl-1)) :#: neg :#: g (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    f 0 = CanviaColor blau :#: Avança 10 -- Si lvl = 0, retorna Avança 10
    f lvl = f (lvl-1) :#: f (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    neg = Gira (-22.5) -- Representa signe '-' de la gramàtica
    pos = Gira 22.5 -- Representa signe '+' de la gramàtica


-- Problema 1 opcional

-- FA AIXÒ:
--axiom = G
--F -> FF
--G -> F[+G]F[-G]+G
--angle = 20

sticks :: Int -> Comanda
sticks lvl = g lvl
  where
    g 0 = CanviaColor blau :#: Avança 10 -- Si lvl = 0, retorna Avança 10
    g lvl = f (lvl-1) :#: Branca (pos :#: g (lvl-1)) :#: f (lvl-1) :#: Branca (neg :#: g (lvl-1)) :#: pos :#: g (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    f 0 = CanviaColor vermell :#: Avança 10 -- Si lvl = 0, retorna Avança 10
    f lvl = f (lvl-1) :#: f (lvl-1) -- Va fent recursivitat amb les crides fins que valgui 0
    neg = Gira (-20)  -- Representa signe '-' de la gramàtica
    pos = Gira 20     -- Representa signe '+' de la gramàtica


-- Problema 2 opcional

-- FA AIXÒ:
-- axiom = F
-- F -> FF-[GH]+[GH]
-- G -> +FH
-- H -> -FG
-- angle = 22.5

weed :: Int -> Comanda
weed lvl = f lvl
  where
    f 0 = CanviaColor vermell :#: Avança 10 -- Si lvl = 0, retorna Avança 10
    f lvl = f (lvl-1) :#: f (lvl-1) :#: neg :#: Branca (g (lvl-1) :#: h (lvl-1)) :#: pos :#: Branca (g (lvl-1) :#: h (lvl-1)) -- Va fent recursivitat amb les crides fins que valgui 0
    g 0 = CanviaColor blau :#: Avança 10 
    g lvl = pos :#: f (lvl-1) :#: h (lvl-1) 
    h 0 = CanviaColor blau :#: Avança 10 
    h lvl = neg :#: f (lvl-1) :#: g (lvl-1) 
    neg = Gira (-22.5)  -- Representa signe '-' de la gramàtica
    pos = Gira 22.5     -- Representa signe '+' de la gramàtica
