{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Marquise
import Control.Monad
main :: IO ()
main = runMarquise "tcp://localhost:5559" 1.2 $
   replicateM_ 100 $ sendCounter [("magic", "counter")] =<< timeNow

