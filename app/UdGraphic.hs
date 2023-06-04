{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module UdGraphic (
    Comanda(..),
    Distancia,
    execute,
    display,
    Llapis (..), blau, verd, vermell, blanc, negre,
    Angle
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0

-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició de les comandes per moure el llapis

type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Comanda :#: Comanda
               | Para
               | CanviaColor Llapis
               | Branca Comanda


-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics

type Turtle = (Llapis, Angle, Pnt) -- Representa un objecte que es mou i dibuixa línies en un pla (Pnt = posició actual)

-- DESCRIPCIÓ: Donat un angle en graus, retorna un Pnt que representa el vector unitari que apunta en la direcció de l'angle
converteix_polar :: Angle -> Pnt
converteix_polar angle = Pnt (cos radiants) (sin radiants)
  where radiants = angle * 2 * pi / 360


-- DESCRIPCIÓ: Donada una comanda, retorna una llista de línies a pintar i la posició final de la tortuga
execute :: Comanda -> [Ln]
execute c = linies
  where 
    (linies, turtle) = processa c (negre, 0, Pnt 0 0) -- La tortuga comença en el punt (0,0) i mirant cap a l'eix positiu de les X

    -- DESCRIPCIÓ: Donada una comanda i una Turtle, retorna una llista de línies a pintar i la posició final de la Turtle
    processa :: Comanda -> Turtle -> ([Ln], Turtle)
    processa (c :#: d) turtle = (liniesC ++ liniesD, turtleD)
      where --Concatenem el que hem de dibuixar de C i de D, i per la Turtle, fem la C i amb el que ens doni el C, fem el D
        (liniesC, turtleC) = processa c turtle
        (liniesD, turtleD) = processa d turtleC
    -- Si és una branca, actualitzem el dibuix però no actualitzem el Turtle (rama principal)
    processa (Branca c) turtle = (liniesC, turtle)
      where (liniesC, turtleC) = processa c turtle
    -- Si avancem --> Actualitzem [Ln] amb el nou dibuix (fa un dibuix amb el llapis de punt que és puntAntic a puntFinal), actualitzem la posició final del Turtle
    processa (Avança dist) (llapis, angle, punt) = ([Ln llapis punt puntFinal | llapis /= Transparent], (llapis, angle, puntFinal))
      where puntFinal = punt + scalar dist * converteix_polar angle
    processa (Gira delta) (llapis, angle, punt) = ([], (llapis, angle-delta, punt)) -- No fem res a la llista Ln, només actualitzem l'angle (Turtle)
    processa (CanviaColor nou) (vell, angle, punt) = ([], (nou, angle, punt)) -- No fem res a la llista Ln, només actualitzem el llapis (Turtle)
    processa Para turtle = ([], turtle) -- No fem res

  -- Rescales all points in a list of lines
  --  from an arbitrary scale
  --  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))
