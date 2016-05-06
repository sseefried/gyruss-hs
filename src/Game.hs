{-# LANGUAGE RecordWildCards #-}
module Game where

import           Control.Arrow
import           Control.Applicative
import           Data.List hiding (group)
import           FRP.Helm
import           FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Mouse as Mouse
import qualified FRP.Helm.Touch as Touch

import           FRP.Elerea.Param hiding (Signal)
import           FRP.Helm.Sample

import Graphics.Rendering.Cairo.Matrix (Matrix(..))

import Debug.Trace

--
-- Constants
--

worldWidth :: Double
worldWidth = 100

shipCircleRadius :: Double
shipCircleRadius = (worldWidth - 15) / 2

laserVelocity = 240 -- per second

--------------

data Input =
  Input {
    inpMousePos   :: (Double, Double) -- in world co-ordinates
  , inpMouseDown  :: Bool
  , inpTouches    :: [(Double, Double)] -- in world co-ordinates
  , inpDelta      :: Time
  } deriving (Read, Show, Eq)


data Ship =
  Ship {
    shipPos :: (Double, Double)
  , shipRot :: Double -- -pi/2 .. pi/2
  , fired   :: Maybe (Double, Double) -- (angle, distance)
  }

data Game =
  Game {
    ship :: Ship
  }

----

mouseToWorld :: (Int,Int) -> (Int,Int) -> (Double,Double)
mouseToWorld (w,h) (x,y) = (((x'/w') - 0.5)* worldWidth, (0.5 - (y'/h')) * worldWidth)
  where
    x' = fromIntegral x
    y' = fromIntegral y
    w' = fromIntegral w
    h' = fromIntegral h


touchToWorld :: (Int,Int) -> (Float,Float) -> (Double,Double)
touchToWorld (w,h) (nx, ny) = ((nx' - 0.5) * worldWidth, (ny' - 0.5) * worldHeight)
  where
    nx' = realToFrac nx
    ny' = realToFrac ny
    w'  = fromIntegral w
    h'  = fromIntegral h
    worldHeight = worldWidth/w'*h'

---

delta :: Signal Time
delta = fmap inSeconds (fps 35)

gameInput :: Signal Input
gameInput = sampleOn delta <|
          (Input
           <$> (mouseToWorld <$> Window.dimensions <*> Mouse.position)
           <*> Mouse.isDown
           <*> (map <$> (touchToWorld <$> Window.dimensions) <*> Touch.positions)
           <*> delta)

stepGame :: Input -> Game -> Game
stepGame inp@(Input {..}) g@(Game {..})= g { ship = ship' }
  where
    Ship{..} = ship
    ship' = ship { shipPos = onCirclePos
                 , shipRot = ang + pi/2
                 , fired = fired' }
    (mx, my) = inpMousePos
    onCirclePos = (shipCircleRadius * cos ang, shipCircleRadius * sin ang)
    ang = atan2 my mx
    fired' =
      case fired of
        Just (ang', d) ->
          if d > shipCircleRadius
           then Nothing
           else Just (ang', d + laserVelocity*inpDelta)
        Nothing ->
          if inpMouseDown then Just (ang, laserVelocity*inpDelta ) else Nothing

gameSignal :: Signal Game
gameSignal = foldp stepGame initState gameInput
  where
    initState = Game { ship = Ship (0,-shipCircleRadius) 0 Nothing }

shipForm :: Ship -> Form
shipForm s@(Ship{..}) =
  group ((move shipPos . rotate shipRot .
                   filled white $ polygon [(-2,0), (2,0), (0,3)]):
          laserForms s)

laserForms :: Ship -> [Form]
laserForms (Ship{..}) =
  case fired of
    Just (ang, d) -> [laserBall 1, laserBall (-1)]
      where
        d' = shipCircleRadius - d
        laserBall dx = move (dx*cos (ang+pi/2),dx*sin (ang+pi/2)) $ move (d'*cos ang, d'*sin ang) $ filled red $ circle 0.7
    Nothing       -> []


render :: Game -> (Int,Int) -> Element
render (Game{..}) (w,h)=
  formsToWorldElement [shipForm ship]
    where
      formsToWorldElement = centeredCollage w h . map (scaleY (-1) . scale sf)
      sf = fromIntegral w/worldWidth

game :: Bool -> IO ()
game isMobile = do
  runAndQuitOnSignal config qPressed $
    render <~ gameSignal ~~ Window.dimensions
  where
    qPressed = Keyboard.isDown Keyboard.QKey
    config =
      case isMobile of
        True -> defaultConfig { windowIsFullscreen = True }
        False ->
          defaultConfig { windowTitle = "Gyruss"
                        , windowIsFullscreen = False
                        , windowDimensions = (winWidth, winHeight)}
    (winWidth, winHeight) = (500, 500)
