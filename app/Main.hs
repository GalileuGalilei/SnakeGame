module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

type Radius = Float 
type Position = (Float, Float)

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity. 
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height.
  } 

initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (1, -3)
  , player1 = 40
  , player2 = -80
  }


-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose 120 $ player1 game,
            mkPaddle orange (-120) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)


moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

fps :: Int
fps = 60

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) radius
          then
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') down _ _) game =
  game {player1 = py + speed}  
  where
    speed = if down==Up then 5
      else -5
    py = player1 game

handleKeys (EventKey (Char 'w') down _ _) game =
  game { player1 = 
    if down==Up then py
      else py + 1}
  where
    py = player1 game
-- Do nothing for all other events.
handleKeys _ game = game


-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds = wallBounce . moveBall (seconds*5)

main :: IO ()
main = play window background fps initialState render handleKeys update