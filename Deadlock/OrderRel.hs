module Deadlock.OrderRel 
    ( OrderRel (..)
    , emptyRel
    , add
    , addLess
    , addLess'
    , addLesses
    , less
    , lessAll
    , lessAll'
    , lessOne
    , greaterOne
    , dom
    , inDom
    , incomparable
    , subOrder
    ) where

import qualified Data.Graph.Inductive as G
-- import qualified Data.Map as Map
-- import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Set (Set)

type Rel a = G.Gr a ()

-- | Relation operations
emptyRel' :: Rel a
emptyRel' = G.empty

data OrderRel a =
  OrderRel { ordRel :: Rel a
           , ordTop :: G.LNode a
           , ordBot :: G.LNode a
           } deriving Show

{-
Internal implementation
-}

nodeId :: Eq a => OrderRel a -> a -> Maybe G.Node
nodeId o x = lookup x (map (\ (a, b) -> (b, a)) (graphNodes o))

graphNodes :: OrderRel a -> [G.LNode a]
graphNodes = G.labNodes . ordRel

hasNode :: Eq a => OrderRel a -> a -> Bool
hasNode o x = isJust (nodeId o x)

connected :: Eq a => OrderRel a -> a -> a -> Bool
connected o a b =
  let Just na = nodeId o a
      Just nb = nodeId o b
  in na `elem` G.pre (ordRel o) nb

elems :: OrderRel a -> [a]
elems (OrderRel o (_, top) (_, bot)) = top : bot : map snd (G.labNodes o)

insNode' :: Eq a => a -> OrderRel a -> OrderRel a
insNode' x o
  | hasNode o x = o
  | otherwise =
    let [i] = G.newNodes 1 (ordRel o)
        n = (i, x)
        add o' = o' {ordRel = G.trc (G.insNode n (ordRel o'))}
        go = insEdge' (snd $ ordBot o) x.
             insEdge' x (snd $ ordTop o) .
             add
    in go o

insEdge' :: Eq a => a -> a -> OrderRel a -> OrderRel a
insEdge' x y o =
  case (nodeId o x, nodeId o y) of
    (Just nx, Just ny) -> o {ordRel = G.trc (G.insEdge (nx, ny, ()) (ordRel o))}
    _ -> error "insEdge': one of the nodes doesn't exist"

unsafeAddLess :: Ord a => a -> a -> OrderRel a -> OrderRel a
unsafeAddLess x y o =
  let go = insEdge' x y .
           insNode' y .
           insNode' x
  in go o


{-
External interface functions
-}

emptyRel :: Ord a => a -> a -> OrderRel a
emptyRel top bot = 
    let
      b = (0, bot)
      t = (1, top)
      g = G.mkGraph [t, b] [(0, 1, ())]
    in OrderRel g t b

add :: (Show a, Ord a) => OrderRel a -> a -> OrderRel a
add o a = insNode' a o

addLesses :: (Show a, Ord a) => OrderRel a -> [(a,a)] -> OrderRel a
addLesses = foldr (uncurry addLess')

addLess :: (Show a, Ord a) => OrderRel a -> a -> a -> OrderRel a
addLess o a b = unsafeAddLess a b o

addLess' :: (Show a, Ord a) => a -> a -> OrderRel a -> OrderRel a
addLess' a b rel = addLess rel a b

addToDom :: Ord a => OrderRel a -> a -> OrderRel a
addToDom = flip insNode'
                               
less :: (Show a, Ord a) => OrderRel a -> a -> a -> Bool
less o a b
  | hasNode o a && hasNode o b = connected o a b
  | hasNode o a = False -- This preserves the old behaviour, which we require
  | otherwise = error $ "less: missing key " ++ show (nodeId o a, nodeId o b, a, b, o)

greater :: (Show a, Ord a) => OrderRel a -> a -> a -> Bool
greater rel a b = less rel b a

greaterOne :: (Show a, Ord a) => OrderRel a -> a -> [a] -> Bool
greaterOne r a = any (greater r a)

lessOne :: (Show a, Ord a) => OrderRel a -> a -> [a] -> Bool
lessOne r a = any (less r a)

lessAll :: (Show a, Ord a) => OrderRel a -> a -> [a] -> Bool
lessAll r a = all (less r a)

lessAll' :: (Show a, Ord a) => OrderRel a -> [a] -> a -> Bool
lessAll' r = flip (lessAll r)

dom :: Ord a => OrderRel a -> Set a
dom o = Set.fromList (map snd (G.labNodes $ ordRel o))

inDom :: Ord a => OrderRel a -> a -> Bool
inDom r a = a `Set.member` dom r

incomparable :: (Show a, Ord a) => OrderRel a -> a -> a -> Bool
incomparable r a b = not (less r a b || less r b a)

instance (Show a, Ord a) => Eq (OrderRel a) where
  OrderRel o1 top1 bot1 == OrderRel o2 top2 bot2 =
    o1 == o2 &&
    top1 == top2 &&
    bot1 == bot2

instance (Show a, Ord a) => Ord (OrderRel a) where
  o1 <= o2 =
    and [ test a b |
          a <- elems o1,
          b <- elems o1]
    where
      test a b
        | hasNode o2 a && hasNode o2 b =
          connected o1 a b <= connected o2 a b
        | otherwise = False

subOrder r1 r2 = r1 <= r2
