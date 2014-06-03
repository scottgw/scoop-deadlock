module Deadlock.DotRel where

import Data.Graph.Inductive hiding (dom)
import Data.GraphViz
import Data.Maybe
import qualified Data.Set as S

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
label = Label . StrLabel . show . snd

renderDot :: (Show a, Ord a) => OrderRel a -> FilePath -> IO ()
renderDot r fp = runGraphviz d Png fp >> return ()
    where d = graphToDot True (relToGraph r) [] ((:[]) . label) (const [])

lessElem :: (Show a, Ord a) => OrderRel a -> [LNode a] -> LNode a -> [LEdge ()]
lessElem r lns a = map pair ls
    where ls = S.toList (S.filter (immLess r (snd a)) (dom r))
          pair n = (lookupNode lns n, fst a, ())

lookupNode :: Eq a => [LNode a] -> a -> Node
lookupNode lns n = fromJust (lookup n (map (\ (a,b) -> (b,a)) lns))

immLess :: (Show a, Ord a) => OrderRel a -> a -> a -> Bool
immLess r a b = S.null (filt (dom r)) && a <* b && a /= b
    where (<*) = less r
          filt = S.filter (\x -> a <* x && x <* b && x /= a && x /= b)

relToGraph :: (Show a, Ord a) => OrderRel a -> Gr a ()
relToGraph r = addEdges r (insNodes lns empty)
    where lns = zip (newNodes (length ns) (empty :: Gr a ())) ns
          ns  = S.toList (dom r)

addEdges :: (Show a, Ord a) => OrderRel a -> Gr a () -> Gr a ()
addEdges r gr = insEdges les gr
    where les = concatMap (lessElem r lns) lns
          lns = labNodes gr