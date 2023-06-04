module Main where

import System.IO
import Artist
import UdGraphic
import Test.QuickCheck

jocProves1 :: IO ()
jocProves1 = do
    putStrLn "Executant joc de prova 1..."
    putStrLn "Separant (Avança 3 :#: Gira 4 :#: Avança 7 :#: Para):"
    print (separa (Avança 3 :#: Gira 4 :#: Avança 7 :#: Para))
    putStrLn "Separant ((Avança 3 :#: Gira 4) :#: (Para :#: Avança 7)):"
    print (separa ((Avança 3 :#: Gira 4) :#: (Para :#: Avança 7)))
    putStrLn "Separant (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7):"
    print (separa (((Para :#: Avança 3) :#: Gira 4) :#: Avança 7))
    putStrLn "Separant (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (separa (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Separant (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para):"
    print (separa (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para))
    putStrLn "Separant (Avança 3 :#: Avança 4 :#: Gira 90 :#: Gira 180 :#: Para):"
    print (separa (Avança 3 :#: Avança 4 :#: Gira 90 :#: Gira 180 :#: Para))
    putStrLn "Separant (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para):"
    print (separa (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para))
    putStrLn "Separant (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para):"
    print (separa (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para))


jocProves2 :: IO ()
jocProves2 = do
    putStrLn "Executant joc de prova 2..."
    putStrLn "Ajuntant [Avança 3, Gira 4, Avança 7]:"
    print (ajunta [Avança 3, Gira 4, Avança 7])
    putStrLn "Ajuntant [Avança 5, Gira 90, Avança 2, Para]:"
    print (ajunta [Avança 5, Gira 90, Avança 2, Para])
    putStrLn "Ajuntant [Gira 180, Avança 10, Gira 45, Para]:"
    print (ajunta [Gira 180, Avança 10, Gira 45, Para])
    putStrLn "Ajuntant [Avança 3, Avança 4, Gira 90, Gira 180, Para]:"
    print (ajunta [Avança 3, Avança 4, Gira 90, Gira 180, Para])
    putStrLn "Ajuntant [Avança 1, Para, Avança 2, Para, Gira 270, Para]:"
    print (ajunta [Avança 1, Para, Avança 2, Para, Gira 270, Para])
    putStrLn "Ajuntant [Gira 45, Gira 90, Gira 135, Gira 180, Gira 225, Gira 270, Gira 315, Para]:"
    print (ajunta [Gira 45, Gira 90, Gira 135, Gira 180, Gira 225, Gira 270, Gira 315, Para])


jocProves31 :: IO ()
jocProves31 = do
    putStrLn "Executant joc de proves 3..."
    putStrLn "Comprovant prop_equivalent (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para) (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (prop_equivalent (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para) (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Comprovant prop_equivalent (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para) (Gira 90 :#: Avança 2 :#: Para):"
    print (prop_equivalent (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para) (Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Comprovant prop_equivalent (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para) (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para):"
    print (prop_equivalent (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para) (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para))
    putStrLn "Comprovant prop_equivalent (Avança 1 :#: Para) (Avança 1 :#: Avança 1 :#: Para):"
    print (prop_equivalent (Avança 1 :#: Para) (Avança 1 :#: Avança 1 :#: Para))
    putStrLn "Comprovant prop_equivalent (Gira 90 :#: Gira 90 :#: Gira 90 :#: Para) (Gira 270 :#: Para):"
    print (prop_equivalent (Gira 90 :#: Gira 90 :#: Gira 90 :#: Para) (Gira 270 :#: Para))


jocProves32 :: IO ()
jocProves32 = do
    putStrLn "Executant joc de proves 3.2..."
    putStrLn "Comprovant prop_separa_ajunta (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (prop_separa_ajunta (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Comprovant prop_separa_ajunta (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para):"
    print (prop_separa_ajunta (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para))
    putStrLn "Comprovant prop_separa_ajunta (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (prop_separa_ajunta (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Comprovant prop_separa_ajunta (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para):"
    print (prop_separa_ajunta (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para))
    putStrLn "Comprovant prop_separa_ajunta (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para):"
    print (prop_separa_ajunta (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para))


jocProves33 :: IO ()
jocProves33 = do
    putStrLn "Executant joc de proves 3.3..."
    putStrLn "Comprovant prop_separa (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para):"
    let comanda1 = Avança 5 :#: Gira 90 :#: Avança 2 :#: Para
    putStrLn "Separa:"
    print (separa comanda1)
    putStrLn "Prop_separa:"
    print (prop_separa comanda1)

    putStrLn "Comprovant prop_separa (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para):"
    let comanda2 = Gira 180 :#: Avança 10 :#: Gira 45 :#: Para
    putStrLn "Separa:"
    print (separa comanda2)
    putStrLn "Prop_separa:"
    print (prop_separa comanda2)

    putStrLn "Comprovant prop_separa (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para):"
    let comanda3 = Avança 3 :#: Gira 90 :#: Avança 2 :#: Para
    putStrLn "Separa:"
    print (separa comanda3)
    putStrLn "Prop_separa:"
    print (prop_separa comanda3)

    putStrLn "Comprovant prop_separa (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para):"
    let comanda4 = Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para
    putStrLn "Separa:"
    print (separa comanda4)
    putStrLn "Prop_separa:"
    print (prop_separa comanda4)

    putStrLn "Comprovant prop_separa (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para):"
    let comanda5 = Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para
    putStrLn "Separa:"
    print (separa comanda5)
    putStrLn "Prop_separa:"
    print (prop_separa comanda5)

    putStrLn "Comprovant prop_separa (Avança 5 :#: Gira 90 :#: Avança 2):"
    let comanda6 = Avança 5 :#: Gira 90 :#: Avança 2
    putStrLn "Separa:"
    print (separa comanda6)
    putStrLn "Prop_separa:"
    print (prop_separa comanda6)

    putStrLn "Comprovant prop_separa (Gira 180 :#: Avança 10 :#: Gira 45):"
    let comanda7 = Gira 180 :#: Avança 10 :#: Gira 45
    putStrLn "Separa:"
    print (separa comanda7)
    putStrLn "Prop_separa:"
    print (prop_separa comanda7)

    putStrLn "Comprovant prop_separa (Avança 3 :#: Gira 90 :#: Avança 2):"
    let comanda8 = Avança 3 :#: Gira 90 :#: Avança 2
    putStrLn "Separa:"
    print (separa comanda8)
    putStrLn "Prop_separa:"
    print (prop_separa comanda8)

    putStrLn "Comprovant prop_separa (Avança 1 :#: Avança 2 :#: Gira 270):"
    let comanda9 = Avança 1 :#: Avança 2 :#: Gira 270
    putStrLn "Separa:"
    print (separa comanda9)
    putStrLn "Prop_separa:"
    print (prop_separa comanda9)

    putStrLn "Comprovant prop_separa (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315):"
    let comanda10 = Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315
    putStrLn "Separa:"
    print (separa comanda10)
    putStrLn "Prop_separa:"
    print (prop_separa comanda10)



jocProves4 :: IO ()
jocProves4 = do
    putStrLn "Executant joc de proves 4..."
    putStrLn "Comprovant copia 3 (Avança 10 :#: Gira 120):"
    print (copia 3 (Avança 10 :#: Gira 120))
    putStrLn "Comprovant copia 3 (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (copia 3 (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Comprovant copia 2 (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para):"
    print (copia 2 (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para))
    putStrLn "Comprovant copia 4 (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (copia 4 (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Comprovant copia 1 (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para):"
    print (copia 1 (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para))


jocProves5 :: IO ()
jocProves5 = do
    putStrLn "Executant joc de proves 5..."
    putStrLn "Comprovant pentagon 10:"
    print (pentagon 10)
    putStrLn "Comprovant pentagon 5:"
    print (pentagon 5)
    putStrLn "Comprovant pentagon 15:"
    print (pentagon 15)
    putStrLn "Comprovant pentagon 8:"
    print (pentagon 8)
    putStrLn "Comprovant pentagon 20:"
    print (pentagon 20)


jocProves5Personal :: IO ()
jocProves5Personal = do
    putStrLn "Executant joc de proves 5..."
    putStrLn "Introdueix la distància:"
    d <- getLine
    let d' = read d :: Float
    print(pentagon d')

jocProves5Grafic :: IO ()
jocProves5Grafic = do
    putStrLn "Executant joc de proves 5..."
    putStrLn "Introdueix la distància:"
    d <- getLine
    let d' = read d :: Float
    display(pentagon d')

jocProves61 :: IO ()
jocProves61 = do
    putStrLn "Executant joc de proves 6.1..."
    putStrLn "Comprovant poligon 10 5 72:"
    print (poligon 10 5 72)
    putStrLn "Comprovant poligon 10 3 120:"
    print (poligon 10 3 120)
    putStrLn "Comprovant poligon 5 4 90:"
    print (poligon 5 4 90)
    putStrLn "Comprovant poligon 15 5 72:"
    print (poligon 15 5 72)
    putStrLn "Comprovant poligon 8 6 60:"
    print (poligon 8 6 60)
    putStrLn "Comprovant poligon 20 8 45:"
    print (poligon 20 8 45)


jocProves61Personal :: IO ()
jocProves61Personal = do
    putStrLn "Executant joc de proves 6.1..."
    putStrLn "Introdueix la distància:"
    d <- getLine
    putStrLn "Introdueix el nombre de costats:"
    n <- getLine
    putStrLn "Introdueix l'angle:"
    a <- getLine
    let d' = read d :: Float
    let n' = read n :: Int
    let a' = read a :: Float
    print(poligon d' n' a')

jocProves61Grafic :: IO ()
jocProves61Grafic = do
    putStrLn "Executant joc de proves 6.1..."
    putStrLn "Introdueix la distància:"
    d <- getLine
    putStrLn "Introdueix el nombre de costats:"
    n <- getLine
    putStrLn "Introdueix l'angle:"
    a <- getLine
    let d' = read d :: Float
    let n' = read n :: Int
    let a' = read a :: Float
    display(poligon d' n' a')


jocProves62 :: IO ()
jocProves62 = do
    putStrLn "Executant joc de proves 6.2..."
    putStrLn "Comprovant prop_poligon_pentagon 5:"
    print (prop_poligon_pentagon 5)
    putStrLn "Comprovant prop_poligon_pentagon 10:"
    print (prop_poligon_pentagon 10)
    putStrLn "Comprovant prop_poligon_pentagon 15:"
    print (prop_poligon_pentagon 15)
    putStrLn "Comprovant prop_poligon_pentagon 8:"
    print (prop_poligon_pentagon 8)
    putStrLn "Comprovant prop_poligon_pentagon 20:"
    print (prop_poligon_pentagon 20)
    putStrLn "Realitzant comprovació amb quickCheck prop_poligon_pentagon:"
    quickCheck prop_poligon_pentagon



jocProves62Personal :: IO ()
jocProves62Personal = do
    putStrLn "Executant joc de proves 6.2..."
    putStrLn "Introdueix el nombre de costats del poligon:"
    n <- getLine
    let ncostats = read n :: Float
    print(prop_poligon_pentagon ncostats)
    
jocProves7 :: IO ()
jocProves7 = do
    putStrLn "Executant joc de proves 7.2..."
    putStrLn "Comprovant espiral 30 5 10 35:"
    print (espiral 30 5 10 35)
    putStrLn "Comprovant espiral 10 3 5 90:"
    print (espiral 10 3 5 90)
    putStrLn "Comprovant espiral (-5) 4 2 45:"
    print (espiral (-5) 4 2 45)
    putStrLn "Comprovant espiral 15 5 (-3) 60:"
    print (espiral 15 5 (-3) 60)
    putStrLn "Comprovant espiral 8 6 0 30:"
    print (espiral 8 6 0 30)
    putStrLn "Comprovant espiral 20 8 (-10) 72:"
    print (espiral 20 8 (-10) 72)
    putStrLn "Comprovant espiral 30 2 (-10) 35:"
    print (espiral 30 2 (-10) 35)



jocProves7Personal :: IO ()
jocProves7Personal = do
    putStrLn "Executant joc de proves 7.2..."
    putStrLn "Introdueix la distància:"
    d <- getLine
    putStrLn "Introdueix el nombre de segments:"
    n <- getLine
    putStrLn "Introdueix el factor d'escala:"
    f <- getLine
    putStrLn "Introdueix l'angle de gir:"
    a <- getLine
    let d' = read d :: Float
    let n' = read n :: Int
    let f' = read f :: Float
    let a' = read a :: Float
    print(espiral d' n' f' a')

jocProves7Grafic :: IO ()
jocProves7Grafic = do
    putStrLn "Executant joc de proves 7.2..."
    putStrLn "Introdueix la distància:"
    d <- getLine
    putStrLn "Introdueix el nombre de segments:"
    n <- getLine
    putStrLn "Introdueix el factor d'escala:"
    f <- getLine
    putStrLn "Introdueix l'angle de gir:"
    a <- getLine
    let d' = read d :: Float
    let n' = read n :: Int
    let f' = read f :: Float
    let a' = read a :: Float
    display(espiral d' n' f' a')

jocProves8 :: IO ()
jocProves8 = do
    putStrLn "Executant joc de proves 8..."
    putStrLn "Executant execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20):"
    print (execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20))
    putStrLn "Executant execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20 :#: Gira (-15) :#: Para :#: Avança 10 :#: Para :#: Para):"
    print (execute (Avança 30 :#: Para :#: Gira 10 :#: Avança 20 :#: Gira (-15) :#: Para :#: Avança 10 :#: Para :#: Para))
    putStrLn "Executant execute (Avança 50 :#: Gira 90 :#: Avança 30 :#: Gira 45 :#: Avança 20 :#: Para):"
    print (execute (Avança 50 :#: Gira 90 :#: Avança 30 :#: Gira 45 :#: Avança 20 :#: Para))
    putStrLn "Executant execute (Avança 10 :#: Gira 180 :#: Avança 20 :#: Gira 45 :#: Para):"
    print (execute (Avança 10 :#: Gira 180 :#: Avança 20 :#: Gira 45 :#: Para))
    putStrLn "Executant execute (Gira 45 :#: Avança 30 :#: Gira 90 :#: Para):"
    print (execute (Gira 45 :#: Avança 30 :#: Gira 90 :#: Para))
    putStrLn "Executant execute (Branca (Avança 20 :#: Gira 90) :#: Gira 45 :#: Avança 30 :#: Para):"
    print (execute (Branca (Avança 20 :#: Gira 90) :#: Gira 45 :#: Avança 30 :#: Para))
    putStrLn "Executant execute (Avança 50 :#: Branca (Gira 180 :#: Avança 20) :#: Avança 30 :#: Gira 45 :#: Para):"
    print (execute (Avança 50 :#: Branca (Gira 180 :#: Avança 20) :#: Avança 30 :#: Gira 45 :#: Para))


jocProves9 :: IO ()
jocProves9 = do
    putStrLn "Executant joc de proves 9..."
    putStrLn "Executant optimitza (Avança 10 :#: Para :#: Avança 20 :#: Gira 35 :#: Avança 0 :#: Gira 15 :#: Gira (-50)):"
    print (optimitza (Avança 10 :#: Para :#: Avança 20 :#: Gira 35 :#: Avança 0 :#: Gira 15 :#: Gira (-50)))
    putStrLn "Executant optimitza (Avança 10 :#: Para :#: Gira 40 :#: Avança 20 :#: Gira (-40)):"
    print (optimitza (Avança 10 :#: Para :#: Gira 40 :#: Avança 20 :#: Gira (-40)))
    putStrLn "Executant optimitza (Avança 10 :#: Para :#: Gira 40 :#: Gira (-30) :#: Avança 20):"
    print (optimitza (Avança 10 :#: Para :#: Gira 40 :#: Gira (-30) :#: Avança 20))
    putStrLn "Executant optimitza (Avança 10 :#: Para :#: Avança (-10)):"
    print (optimitza (Avança 10 :#: Para :#: Avança (-10)))
    putStrLn "Executant optimitza (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (optimitza (Avança 5 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Executant optimitza (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para):"
    print (optimitza (Gira 180 :#: Avança 10 :#: Gira 45 :#: Para))
    putStrLn "Executant optimitza (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para):"
    print (optimitza (Avança 3 :#: Gira 90 :#: Avança 2 :#: Para))
    putStrLn "Executant optimitza (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para):"
    print (optimitza (Avança 1 :#: Para :#: Avança 2 :#: Para :#: Gira 270 :#: Para))
    putStrLn "Executant optimitza (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para):"
    print (optimitza (Gira 45 :#: Gira 90 :#: Gira 135 :#: Gira 180 :#: Gira 225 :#: Gira 270 :#: Gira 315 :#: Para))
    putStrLn "Executant optimitza (Avança 5 :#: Gira 90 :#: Avança 2 :#: Gira 0 :#: Avança 0 :#: Gira 45 :#: Para):"
    print (optimitza (Avança 5 :#: Gira 90 :#: Avança 2 :#: Gira 0 :#: Avança 0 :#: Gira 45 :#: Para))
    putStrLn "Executant optimitza (Gira 180 :#: Avança 10 :#: Gira 0 :#: Gira 0 :#: Avança 0 :#: Gira 45 :#: Para):"
    print (optimitza (Gira 180 :#: Avança 10 :#: Gira 0 :#: Gira 0 :#: Avança 0 :#: Gira 45 :#: Para))
    putStrLn "Executant optimitza (Avança 3 :#: Gira 90 :#: Avança 0 :#: Avança 0 :#: Gira 0 :#: Gira 0 :#: Avança 2 :#: Para):"
    print (optimitza (Avança 3 :#: Gira 90 :#: Avança 0 :#: Avança 0 :#: Gira 0 :#: Gira 0 :#: Avança 2 :#: Para))
    putStrLn "Executant optimitza (Avança 1 :#: Para :#: Avança 0 :#: Avança 0 :#: Gira 270 :#: Para):"
    print (optimitza (Avança 1 :#: Para :#: Avança 0 :#: Avança 0 :#: Gira 270 :#: Para))
    putStrLn "Executant optimitza (Gira 45 :#: Gira 90 :#: Avança 0 :#: Gira 0 :#: Avança 0 :#: Gira 315 :#: Gira 0 :#: Gira 0 :#: Para):"
    print (optimitza (Gira 45 :#: Gira 90 :#: Avança 0 :#: Gira 0 :#: Avança 0 :#: Gira 315 :#: Gira 0 :#: Gira 0 :#: Para))


jocProves10 :: IO ()
jocProves10 = do
    putStrLn "Executant joc de proves 10 (triangle 1)..."
    print(triangle 1)

jocProves10Personal :: IO ()
jocProves10Personal = do
    putStrLn "Executant joc de proves 10..."
    putStrLn "Introdueix el nivell:"
    d <- getLine
    let d' = read d :: Int
    display (triangle d')

jocProves11 :: IO ()
jocProves11 = do
    putStrLn "Executant joc de proves 11 (fulla 1)..."
    print(fulla 1)

jocProves11Personal :: IO ()
jocProves11Personal = do
    putStrLn "Executant joc de proves 11..."
    putStrLn "Introdueix el nivell:"
    d <- getLine
    let d' = read d :: Int
    display (fulla d')

jocProves12 :: IO ()
jocProves12 = do
    putStrLn "Executant joc de proves 12 (hilbert 1)..."
    print(hilbert 1)

jocProves12Personal :: IO ()
jocProves12Personal = do
    putStrLn "Executant joc de proves 12..."
    putStrLn "Introdueix el nivell:"
    d <- getLine
    let d' = read d :: Int
    display (hilbert d')

jocProves13 :: IO ()
jocProves13 = do
    putStrLn "Executant joc de proves 13 (fletxa 1)..."
    print(fletxa 1)

jocProves13Personal :: IO ()
jocProves13Personal = do
    putStrLn "Executant joc de proves 13..."
    putStrLn "Introdueix el nivell:"
    d <- getLine
    let d' = read d :: Int
    display (fletxa d')

jocProves14 :: IO ()
jocProves14 = do
    putStrLn "Executant joc de proves 14 (branca 1)..."
    print(branca 1)

jocProves14Personal :: IO ()
jocProves14Personal = do
    putStrLn "Executant joc de proves 14..."
    putStrLn "Introdueix el nivell:"
    d <- getLine
    let d' = read d :: Int
    display (branca d')

jocProves15 :: IO ()
jocProves15 = do
    putStrLn "Executant joc de proves Opcional 1 (sticks 1)..."
    print(sticks 1)

jocProves15Personal :: IO ()
jocProves15Personal = do
    putStrLn "Executant joc de proves Opcional 1..."
    putStrLn "Introdueix el nivell:"
    d <- getLine
    let d' = read d :: Int
    display (sticks d')

jocProves16 :: IO ()
jocProves16 = do
    putStrLn "Executant joc de proves Opcional 2 (weed 1)..."
    print(weed 1)

jocProves16Personal :: IO ()
jocProves16Personal = do
    putStrLn "Executant joc de proves Opcional 2..."
    putStrLn "Introdueix el nivell:"
    d <- getLine
    let d' = read d :: Int
    display (weed d')

-- Funció principal

main :: IO ()
main = do
    putStrLn "Seleccioni un joc de proves:"
    putStrLn "1. joc de proves 1"
    putStrLn "2. joc de proves 2"
    putStrLn "31. joc de proves 3.1"
    putStrLn "32. joc de proves 3.2"
    putStrLn "33. joc de proves 3.3"
    putStrLn "4. joc de proves 4"
    putStrLn "5. joc de proves 5"
    putStrLn "55. joc de proves 5 personalitzat"
    putStrLn "555. joc de proves 5 (Grafic)"
    putStrLn "61. joc de proves 6.1"
    putStrLn "611. joc de proves 6.1 personalitzat"
    putStrLn "6111. joc de proves 6.1 (Grafic)"
    putStrLn "62. joc de proves 6.2"
    putStrLn "622. joc de proves 6.2 personalitzat"
    putStrLn "7. joc de proves 7"
    putStrLn "77. joc de proves 7 personalitzat"
    putStrLn "777. joc de proves 7 (Grafic)"
    putStrLn "8. joc de proves 8"
    putStrLn "9. joc de proves 9"
    putStrLn "10. joc de proves 10 (Display)"
    putStrLn "100. joc de proves 10 (Print)"
    putStrLn "11. joc de proves 11 (Display)"
    putStrLn "111. joc de proves 11 (Print)"
    putStrLn "12. joc de proves 12 (Display)"
    putStrLn "122. joc de proves 12 (Print)"
    putStrLn "13. joc de proves 13 (Display)"
    putStrLn "133. joc de proves 13 (Print)"
    putStrLn "14. joc de proves 14 (Display)"
    putStrLn "144. joc de proves 14 (Print)"
    putStrLn "15. OPCIONAL 1. joc de proves Opcional 1 (Display)"
    putStrLn "155. OPCIONAL 1. joc de proves Opcional 1 (Print)"
    putStrLn "16. OPCIONAL 2. joc de proves Opcional 2 (Display)"
    putStrLn "166. OPCIONAL 1. joc de proves Opcional 2 (Print)"
    putStrLn "0. Sortir"

    opcion <- getLine
    case opcion of
        "1" -> jocProves1
        "2" -> jocProves2
        "31" -> jocProves31
        "32" -> jocProves32
        "33" -> jocProves33
        "4" -> jocProves4
        "5" -> jocProves5
        "55" -> jocProves5Personal
        "555" -> jocProves5Grafic
        "61" -> jocProves61
        "611" -> jocProves61Personal
        "6111" -> jocProves61Grafic
        "62" -> jocProves62
        "622" -> jocProves62Personal
        "7" -> jocProves7
        "77" -> jocProves7Personal
        "777" -> jocProves7Grafic
        "8" -> jocProves8
        "9" -> jocProves9
        "10" -> jocProves10
        "100" -> jocProves10Personal
        "11" -> jocProves11
        "111" -> jocProves11Personal
        "12" -> jocProves12
        "122" -> jocProves12Personal
        "13" -> jocProves13
        "133" -> jocProves13Personal
        "14" -> jocProves14
        "144" -> jocProves14Personal
        "15" -> jocProves15
        "155" -> jocProves15Personal
        "16" -> jocProves16
        "166" -> jocProves16Personal

        "0" -> putStrLn "Sortim..."
        _   -> putStrLn "Opció invàlida"