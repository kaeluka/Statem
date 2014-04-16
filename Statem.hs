{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Statem (show,
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

import Control.Monad
import Data.List

newtype Tst a = Tst (a->Bool)

newtype Transition i t a = Transition (State i, t, State i)

type DFA i t a = Statem Maybe i t a
type NFA i t a = Statem [] i t a

-- A predicate implements check, which takes a value and returns a function from the alphabet to Bool
class Predicate b a where
  check :: b -> a -> Bool

instance Predicate (Tst a) a where
  check (Tst tst) = tst

instance (Ord a) => Predicate a a where
  check = (==)

instance Show (Tst a) where
  show _ = "?"

instance (Show i, Show t) => Show (Transition i t a) where
  show (Transition (s, tst, s')) =
    (show s) ++ " -" ++ show tst ++ "-> " ++ (show s')

data State i = State i deriving (Show,Eq)

info :: State i -> i
info (State i) = i

data Statem m i t a = Statem { transitions :: [Transition i t a],
                               current :: m (State i)}

instance (Show i, Show t, Show (m (State i))) => Show (Statem m i t a) where
  show st = "Statem { " ++ (show (transitions st)) ++ ", " ++ show (current st) ++ " }"

state i = State i

empty :: MonadPlus m => m (State i) -> Statem m i t a
empty start = Statem { transitions = [], current = start }

chainFrom :: MonadPlus m => State i -> [(t, State i)] -> Statem m i t a
chainFrom start tss = 
  let
    (ret, _) =
      foldl' (\ (acc, lastState) (trans, newState) ->
               (connect lastState trans newState acc, newState))
      (empty $ return start, start)
      tss
  in ret

connect :: State i -> t -> State i -> Statem m i t a  -> Statem m i t a
connect s t s' sm = sm { transitions = (Transition (s, t, s')) : (transitions sm) }

cur_transitions :: (MonadPlus m, Eq i) => Statem m i t a -> m (Transition i t a)
cur_transitions (Statem {current = cur, transitions = ts}) = do
  c <- cur
  msum $ map return $ filter (\(Transition (s,_,_))-> s==c) ts

new_state :: (Predicate t a, MonadPlus m, Eq i) => a -> Statem m i t a -> m (State i)
new_state x st = do
  (Transition (_,t,s)) <- cur_transitions st
  guard (check t $ x)
  return s

applyAll :: (Predicate t a, MonadPlus m, Eq i) => [a] -> Statem m i t a -> Statem m i t a
applyAll xs st = foldl' (\st x -> apply x st) st xs

apply :: (Predicate t a, MonadPlus m, Eq i) => a -> Statem m i t a -> Statem m i t a
apply x st = do
  st { current = new_state x st }

