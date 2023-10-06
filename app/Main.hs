{-# OPTIONS -Wall #-}
module Main where

import Raylib.Core (clearBackground, getRenderWidth, getRenderHeight)
import Raylib.Core.Text (drawText)
import Raylib.Util (drawing, whileWindowOpen0, withWindow)
import Raylib.Util.Colors (lightGray, rayWhite, black)
import Raylib.Core.Shapes (drawCircle, drawRectangle)

import System.Random (randomRIO)

--gridSize as width height
gridSize :: (Int, Int)
gridSize = (10, 10)

numCells :: Int
numCells = 20

randomList :: Int -> Int -> IO([Int])
randomList maxValue 0 = return []
randomList maxValue n = do
  r <- randomRIO (0, maxValue)
  rs <- randomList maxValue (n-1)
  return (r:rs)

renderGrid :: (Int, Int) -> [Int] -> [Int] -> IO ()
renderGrid cellSize [] _ = return ()
renderGrid cellSize _ [] = return ()
renderGrid cellSize (x:xs) (y:ys) = do
  drawRectangle (x * (fst cellSize)) (y * (snd cellSize)) (fst cellSize) (snd cellSize) black
  renderGrid cellSize xs ys


main :: IO ()
main = do
  ys <- randomList (10) numCells :: IO [Int]
  xs <- randomList (10) numCells :: IO [Int]
  withWindow
    600
    450
    "The Scrapyard"
    60
    ( \_ -> do

        whileWindowOpen0
          ( drawing
              ( do
		  screenWidth <- getRenderWidth :: IO Int
		  screenHeight <- getRenderHeight :: IO Int

	          let cellSize = (screenWidth `div` (fst gridSize), screenHeight `div` (snd gridSize))

                  clearBackground rayWhite

		  renderGrid cellSize xs ys
              )
          )
    )

