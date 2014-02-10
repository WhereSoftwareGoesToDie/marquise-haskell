{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Marquise
import Control.Monad
import Control.Applicative
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as B
import Criterion.Main

randomTags :: [ByteString] -> [(ByteString, ByteString)]
randomTags randoms =
    liftM2 (,) randoms randoms

sendInts :: Int -> Int -> IO ()
sendInts n_objs ints_per_obj = do
    ls <- B.lines <$> B.readFile "/usr/share/dict/words"
    let pairs = take n_objs $ randomTags ls

    runMarquise "tcp://localhost:5560" 1.2 $ do
        time <- timeNow
        forM_ pairs $ \pair ->
            replicateM_ ints_per_obj $
                sendInt [pair] time 1234
main :: IO ()
main = do
    defaultMain
        [ bench "1000 oids" $ nfIO $ sendInts 1000 100
        ]
