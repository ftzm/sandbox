module Bench where

import Prelude hiding (readFile, lookup, null, length)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C
import System.Random
import Control.DeepSeq(force)
import Control.Exception(evaluate)
import Control.Monad
import System.TimeIt
import Data.Time.Clock.POSIX (getPOSIXTime)
import Criterion.Main
import qualified Data.Trie as T
import Data.ByteString (readFile, split, ByteString, length)

import Boggler
import NFTrie

-------------------------------------------------------------------------------
-- Setup

genRandomBoard :: IO (Board, String)
genRandomBoard = do
  g <- newStdGen
  return $ (\x ->  (M.fromList $ zip boardCoords $ map C.singleton x, x))
         $ take 16 (randomRs ('a', 'z') g)

dup x = (x,x)

getWordsTrie :: IO (T.Trie ByteString)
getWordsTrie = T.fromList . map dup . C.split '\n' <$> readFile "words_alpha"

-------------------------------------------------------------------------------
-- Runners

benchMain' = do
  t <- evaluate =<< force <$> getWordsTrie
  bs <- evaluate =<< force <$> replicateM 10 genRandomBoard
  defaultMain [
    env (pure ()) $ \ ~() ->
        bgroup "main" [bench label $ whnf (uncurry getBest) (t,b)
                      | ~(b, label) <- bs]
    ]


benchMain = do
  boards <- evaluate =<< force <$> replicateM 6000 genRandomBoard
  t <- evaluate =<< force <$> getWordsTrie

  before <- (round . (* 1000)) <$> getPOSIXTime

  timeIt $ forM_ boards $ \(b, label) ->
    mapM_ putStrLn [ take 4 label
                   , take 4 $ drop 4 label
                   , take 4 $ drop 8 label
                   , drop 12 label
                   , (show $ getBest t b)
                   , ""
                   ]
  after <- (round . (* 1000)) <$> getPOSIXTime
  putStrLn $ "Real Seconds Elapsed: " ++ (show $ after - before)
