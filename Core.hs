{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Core (show,
             State,
             Statem (transitions,current),
             connect,
             info,
             state,
             out_transitions,
             allStates,
             empty,
             chainFrom,
             apply,
             applyAll) where

import Control.Monad
import Data.List

newtype Tst a = Tst (a->Bool)

newtype Transition i t a = Transition (State i, t, State i)

-- A predicate implements check, which takes a value and returns a function from the alphabet to Bool
class Predicate b a where
  check :: b -> a -> Bool

instance Predicate (Tst a) a where
  check (Tst tst) = tst

instance (Eq a) => Predicate a a where
  check = (==)

data State i = State i deriving (Show,Eq)

info :: State i -> i
info (State i) = i

data Statem m i t a = Statem { transitions :: [Transition i t a],
                               current :: m (State i)}

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

connect :: State i -> t -> State i -> Statem m i t a -> Statem m i t a
connect s t s' sm = sm { transitions = (Transition (s, t, s')) : (transitions sm) }

out_transitions :: (Eq i) => State i -> Statem m i t a -> [Transition i t a]
out_transitions s statem = filter (\(Transition (s',_,_))-> s==s') (transitions statem)

allStates statem = foldl' (\states (Transition (s,_,s')) -> s:s':states) [] (transitions statem)

new_state :: (Predicate t a, MonadPlus m, Eq i) => a -> Statem m i t a -> m (State i)
new_state x statem = do
  st' <- current statem
  (Transition (_,t,s)) <- msum $ map return $ out_transitions st' statem
  guard (check t $ x)
  return s

applyAll :: (Predicate t a, MonadPlus m, Eq i) => [a] -> Statem m i t a -> Statem m i t a
applyAll xs st = foldl' (\st x -> apply x st) st xs

apply :: (Predicate t a, MonadPlus m, Eq i) => a -> Statem m i t a -> Statem m i t a
apply x st = do
  st { current = new_state x st }

-- CONVERSION

isAmbiguous :: (Eq t, Eq i) => Transition i t a -> Statem m i t a -> Bool
isAmbiguous (Transition (sa, t, sb)) nfa =
  let targets = filter (\(Transition (sa',t',sb')) -> sa == sa' && t == t' && sb /= sb') (transitions nfa)
  in (length targets) > 1

-- PRETTY PRINTING

instance Show (Tst a) where
  show _ = "?"

instance (Show i, Show t) => Show (Transition i t a) where
  show (Transition (s, tst, s')) =
    (show s) ++ " -" ++ show tst ++ "-> " ++ (show s')

instance (Show i, Show t, Show (m (State i))) => Show (Statem m i t a) where
  show st = "Statem { " ++ (show (transitions st)) ++ ", " ++ show (current st) ++ " }"
