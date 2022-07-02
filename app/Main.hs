module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import GHC.IO.Encoding (TextEncoding(mkTextEncoder))

width, height, cols, rows, offset :: Int
width = 480
height = 480
cols = 32
rows = 32
offset = 10

window :: Display
window = InWindow "SnakeGame" (width + offset, height + offset) (0,0)

background :: Color
background = black

data GameState = State
  { snake :: [Point],
    snakeDir :: Point,
    foodLoc :: Point,
    isGameover :: Bool
  }

initialState :: GameState
initialState = State
  { snake = [(fX, fY), (fX + 1, fY),(fX + 2, fY), (fX + 3, fY)],
    snakeDir = (1,0),
    foodLoc = (20,20), 
    isGameover = False
  }
  where 
    fX = fromIntegral rows / 2
    fY = fromIntegral cols / 2


-- | Convert a game state into a picture.
render :: GameState -> Picture  
render game = pictures (food : (snakeBody ++ walls)) 
  where
    food = mkNode (dark red) (foodLoc game)
    snakeBody = map (mkNode white) (snake game)

    -- paredes do topo e dos lados
    walls :: [Picture]
    walls = 
      [
      (translate halfX 0 $ color wallColor $ rectangleSolid offsetf h),
      (translate (-halfX) 0 $ color wallColor $ rectangleSolid offsetf h),
      (translate 0 halfY $ color wallColor $ rectangleSolid w offsetf),
      (translate 0 (-halfY) $ color wallColor $ rectangleSolid w offsetf)
      ]
        where
          halfX = fromIntegral (width + offset) / 2
          halfY = fromIntegral (height + offset) / 2
          w = fromIntegral (width + 2 * offset) 
          h = fromIntegral (height + 2 * offset)
          wallColor = white
          offsetf = fromIntegral offset


    --cria um retângulo(nó) da cobra
    mkNode :: Color -> Point -> Picture
    mkNode col (x, y) = translate xt yt $ color col $ rectangleSolid rectX rectY
      where
        xt = x*rectX - fromIntegral (width + offset) / 2
        yt = y*rectY - fromIntegral (height + offset) / 2
        rectX = fromIntegral (width + offset) / (fromIntegral cols)
        rectY = fromIntegral (height + offset) / (fromIntegral rows)



fps :: Int
fps = 60







main :: IO ()
main = display window background (render initialState)