module Statem
       (Statem,
        Trans,
        AcceptMode (..),
        mkState,
        state_name,
        apply,
        connect,
        tie,
        statem_main,
        Edge
       ) where

import Test.QuickCheck
import Control.Monad

----------- STATEM

type Trans a = (a->Bool)
newtype Edge a = Edge (Trans a, Statem a)
data Meta = Meta { name :: String,
                   accept_mode :: AcceptMode } deriving (Show,Eq)
data Statem a = SM Meta [Edge a]

data AcceptMode = Accepting | Nonaccepting deriving (Show, Eq)

--instance Arbitrary (Int -> Bool) where
--  arbitrary = (arbitrary :: Gen Int) >>= \i -> return (edge_eq i)
--
--arbSt :: Int -> Gen Statem Int
--arbSt 0 = arbitrary :: Gen String >>= \s -> mkState s
--arbSt n = let n_2 = div n 2
--              r = n - n_2
--          in 
--
--instance Arbitrary (Statem Int) where
--  arbitrary = (arbitrary :: Gen Int) >>=
--              \i -> case i of
--                0 -> mk

meta :: Statem a -> Meta
meta (SM m _) = m

edges :: Statem a -> [Edge a]
edges (SM _ es) = es

edge_eq :: Eq a => a -> a -> Bool
edge_eq x = ((==) x)

mkState n am = SM (Meta { name=n, accept_mode=am }) []

apply :: MonadPlus m => a -> Statem a -> m (Statem a)
apply x (SM m edges) =
  let
    valid = map (\(Edge (_,s)) -> s) $ filter (\ (Edge (f,_)) -> f x) $ edges
    rvs = map return valid
  in
   msum rvs

state_name :: Statem a -> String
state_name (SM m _) = name m

-- take two tests and two SMs and make a cycle
tie :: (Statem a) -> (a -> Bool) -> (a -> Bool) -> Statem a -> Statem a
tie (SM m1 es1) f1 f2 (SM m2 es2) =
  let s1' = SM m1 (Edge (f1, s2') : es1)
      s2' = SM m2 (Edge (f2, s1') : es2)
  in s1'

connect :: (Statem a) -> (Trans a) -> (Statem a) -> (Statem a)
connect (SM m es) f s2 = SM m (Edge (f,s2):es)

prop_connect x = let a = mkState "a" Accepting
                     b = mkState "b" Accepting
                     conn = connect a (edge_eq x) b
                 in (apply x conn >>= Just . meta) == Just (meta b)

prop_tie_cycle x y =
  let sa' = mkState "sa'" Accepting
      sb' = mkState "sb'" Accepting
      knot = (tie sa' (edge_eq x) (edge_eq y) sb')
  in ((apply x knot) >>= (apply y) >>= Just . meta) == Just (meta sa')

prop_loop x =
  let s = mkState "!!" Accepting
      s' = tie s (edge_eq x) (edge_eq x) s
  in (apply x s'
      >>= (apply x)
      >>= (apply x)
      >>= (apply x)
      >>= (apply x)
      >>= Just . meta) == Just (meta s)


statem_main = do
  quickCheck (prop_connect :: Int -> Bool)
  quickCheck (prop_tie_cycle :: Int -> Int -> Bool)
  quickCheck (prop_loop :: Int -> Bool)

main = statem_main
