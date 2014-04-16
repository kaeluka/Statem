module Tracked (asDFA,
                DFA,
                NFA,
                dfa_connect,
                nfa_connect,
                C.state,
                C.applyAll,
                C.chainFrom
               ) where

import qualified Core as C

type DFA i t a = C.Statem Maybe i t a
type NFA i t a = C.Statem [] i t a


nfa_connect :: C.State i -> t -> C.State i -> NFA i t a -> NFA i t a
nfa_connect = C.connect

dfa_connect :: (Eq t, Eq i) => C.State i -> t -> C.State i -> DFA i t a -> Maybe (DFA i t a)
dfa_connect s t s' dfa = asDFA $ C.connect s t s $ asNFA dfa

stateHasAmbiguous :: (Eq t, Eq i) => C.Statem m i t a -> C.State i -> Bool
stateHasAmbiguous statem s = (length $ C.out_transitions s statem) > 1

nfaHasAmbiguous :: (Eq t, Eq i) => NFA i t a -> Bool
nfaHasAmbiguous statem = any (stateHasAmbiguous statem) $ C.allStates statem

asDFA :: (Eq t, Eq i) => NFA i t a -> Maybe (DFA i t a)
asDFA nfa = if nfaHasAmbiguous nfa && (length (C.current nfa)<=1)
            then Nothing
            else 
              Just $ (C.empty (Just $ head (C.current nfa))) { C.transitions = (C.transitions nfa)}

asNFA :: DFA i t a -> NFA i t a
asNFA dfa = (case (C.current dfa) of
                Just s ->  (C.empty [s])
                Nothing -> (C.empty [])) { C.transitions = (C.transitions dfa) }
