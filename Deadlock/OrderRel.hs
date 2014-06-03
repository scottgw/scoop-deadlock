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

import qualified Data.Map as M
import Data.Map (Map)
import Data.Set as S (Set, isSubsetOf, insert, empty, member
                , singleton, filter, null, toList, foldl)

type Rel a = Map a (Set a)
data OrderRel a = OrderRel { ordMap :: Rel a
                           , ordTop :: a
                           , ordBot :: a
                           } deriving Show

{-
Internal implementation
-}

updMap :: (Rel a -> Rel a) -> OrderRel a -> OrderRel a
updMap f o = o {ordMap = f (ordMap o)}

liftMap :: (Rel a -> b) -> OrderRel a -> b
liftMap = (. ordMap)

ordInsert :: Ord a => a -> Set a -> OrderRel a -> OrderRel a
ordInsert k v = updMap (M.insert k v)

ordLookup :: Ord a => a -> OrderRel a -> Maybe (Set a)
ordLookup k = liftMap (M.lookup k)

oneInsert :: Ord a => a -> a -> OrderRel a -> OrderRel a
oneInsert k v rel 
    | k == v    = rel
    | otherwise = ordInsert k (insert v s) rel
    where
      s = maybe (error "oneInsert: no key") id (ordLookup k rel)

between :: Ord a => a ->  OrderRel a -> OrderRel a
between k rel = (oneInsert k (ordTop rel) . oneInsert (ordBot rel) k) rel

unsafeAddLess :: Ord a => OrderRel a -> a -> a -> OrderRel a
unsafeAddLess rel a b
    | a /= b    = (between a . between b . oneInsert a b) rel'
    | otherwise = rel'
    where rel' = addToDom (addToDom rel b) a


{-
External interface functions
-}

emptyRel :: Ord a => a -> a -> OrderRel a
emptyRel top bot = 
    let r = (M.insert top empty . M.insert bot (singleton top)) M.empty
    in OrderRel r top bot

add :: (Show a, Ord a) => OrderRel a -> a -> OrderRel a
add r a = addLess r a (ordTop r)

addLesses :: (Show a, Ord a) => OrderRel a -> [(a,a)] -> OrderRel a
addLesses = foldr (uncurry addLess')

addLess :: (Show a, Ord a) => OrderRel a -> a -> a -> OrderRel a
addLess r a b 
    | not (inDom r a && inDom r b) = unsafeAddLess r a b
    | less r b a && a /= b = error "safeAddLess: b already less than a"
    | otherwise            = unsafeAddLess r a b

addLess' :: (Show a, Ord a) => a -> a -> OrderRel a -> OrderRel a
addLess' a b rel = addLess rel a b

addToDom :: Ord a => OrderRel a -> a -> OrderRel a
addToDom rel a = maybe (ordInsert a empty rel) (const rel) (ordLookup a rel)
                               
less :: (Show a, Ord a) => OrderRel a -> a -> a -> Bool
less rel a c
    | a == c || c `member` img = True
    | otherwise                = not . S.null . S.filter (greater rel c) $ img
    where img = case ordLookup a rel of
                  Nothing -> error $ "less: no key " ++ show a
                  Just i  -> i

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
dom = liftMap M.keysSet

inDom :: Ord a => OrderRel a -> a -> Bool
inDom r a = a `member` dom r

incomparable :: (Show a, Ord a) => OrderRel a -> a -> a -> Bool
incomparable r a b = not (less r a b || less r b a)

instance (Show a, Ord a) => Eq (OrderRel a) where
    oa == ob 
        | dom oa == dom ob = all (uncurry imp) axa
        | otherwise        = False
        where
          imp a b = less oa a b == less ob a b
          as  = toList (dom oa)
          axa = [(a1,a2) | a1 <- as, a2 <- as]

instance (Show a, Ord a) => Ord (OrderRel a) where
    a <= b 
        | dom a `isSubsetOf` dom b = all (uncurry imp) axa
        | otherwise      = False
        where
          imp x y = less a x y <= less b x y
          as  = toList (dom a)
          axa = [(a1,a2) | a1 <- as, a2 <- as]

setAll :: Ord a => (a -> Bool) -> Set a -> Bool
setAll p = S.foldl (\ acc x -> p x && acc) True

setAllPair :: Ord a => (a -> a -> Bool) -> Set a -> Bool
setAllPair p as = setAll (\ x -> setAll (\y -> p x y) as) as

subOrder :: (Show a, Ord a) => OrderRel a -> OrderRel a -> Bool
subOrder a b = setAllPair (\ x y  -> less a x y <= less b x y) as
  where
    as = dom a


-- subOrder a b 
--     | dom a `isSubsetOf` dom b = all (uncurry imp) axa
--     | otherwise      = False
--     where
--       imp x y = less a x y <= less b x y
--       as  = toList (dom a)
--       axa = [(a1,a2) | a1 <- as, a2 <- as]
