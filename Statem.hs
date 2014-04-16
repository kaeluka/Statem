{-# LANGUAGE FlexibleContexts #-}

module Statem (Edge,
               show,
               State,
               Statem (current,transitions),
               cur_transitions,
               DFA,
               NFA,
               info,
               state,
               empty,
               connect,
               chainFrom,
               apply,
               applyAll) where

import Test.QuickCheck
import Control.Monad
import Data.List

----------- STATEM

type Edge a = (a->Bool)
newtype Transition i a = Transition (State i, Edge a, State i)

type DFA i a = Statem Maybe i a
type NFA i a = Statem [] i a

instance Show i => Show (Transition i a) where
  show (Transition (s, _, s')) =
    (show s) ++ " -?-> " ++ (show s')

data State i = State i deriving (Show,Eq)

info :: State i -> i
info (State i) = i

data Statem m i a = Statem { transitions :: [Transition i a],
                             current :: m (State i)}

instance (Show i, Show (m (State i))) => Show (Statem m i a) where
  show st = "Statem { " ++ (show (transitions st)) ++ ", " ++ show (current st) ++ " }"

--instance Show (m (State i)) => Show (Statem m i a) where
--  show = "Statem { " ++ show transitions ++ ", " ++ show current 

state i = State i

empty :: MonadPlus m => m (State i) -> Statem m i a
empty start = Statem { transitions = [], current = start }

chainFrom :: MonadPlus m => State i -> [(Edge a, State i)] -> Statem m i a 
chainFrom start tss = 
  let
    (ret, _) =
      foldl' (\ (acc, lastState) (trans, newState) ->
               (connect lastState trans newState acc, newState))
      (empty $ return start, start)
      tss
  in ret

connect :: State i -> Edge a -> State i -> Statem m i a  -> Statem m i a
connect s t s' sm = sm { transitions = (Transition (s, t, s')):(transitions sm) }

cur_transitions :: (MonadPlus m) => (Eq i) => Statem m i a -> m (Transition i a)
cur_transitions (Statem {current = cur, transitions = ts}) = do
  c <- cur
  msum $ map return $ filter (\(Transition (s,_,_))-> s==c) ts

applyAll :: MonadPlus m => Eq i => [a] -> Statem m i a -> m (Statem m i a)
applyAll xs st = foldl' (\mst x -> mst >>= \ st -> apply x st) (return st) xs

apply :: MonadPlus m => Eq i => a -> Statem m i a -> m (Statem m i a)
apply x st = do
  (Transition (_,t,s)) <- cur_transitions st
  if t x then return st { current = return s } else mzero

