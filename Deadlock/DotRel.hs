module Deadlock.DotRel (generateImage) where

import Control.Monad

import Data.Graph.Inductive hiding (dom)
import Data.GraphViz

import System.IO

import Deadlock.OrderRel

tmpFileName, tmpFileDir, imgExt :: String
tmpFileName = "relation"
tmpFileDir = "/tmp"
imgExt = ".png"

generateImage :: (Show a, Ord a) => OrderRel a -> IO FilePath
generateImage r = do
  (outFile, hOut) <- openTempFile tmpFileDir (tmpFileName ++ imgExt)
  hClose hOut
  renderDot r outFile
  return outFile

label :: Show a => LNode a  -> Attribute
label = toLabel . show . snd

renderDot :: (Show a, Ord a) => OrderRel a -> FilePath -> IO ()
renderDot r fp = void $ runGraphviz d Png fp
    where d = graphToDot (nonClusteredParams {fmtNode = \ n -> [label n]})
                         (ordRel r)
