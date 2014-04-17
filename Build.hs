module Build where

import Control.Monad
import Core
import Data.List
import Data.Monoid

chainFrom :: MonadPlus m => State i -> [(t, State i)] -> Statem m i t a
chainFrom start tss = 
  let
    (ret, _) =
      foldl' (\ (acc, lastState) (trans, newState) ->
               (connect lastState trans newState acc, newState))
      (empty $ return start, start)
      tss
  in ret

(<+>) :: MonadPlus m => Statem m i t a -> Statem m i t a -> Statem m i t a
a <+> b =
  (empty ((current a) `mplus` (current b))) {
    transitions = (transitions a) ++ transitions b
    }

instance MonadPlus m => Monoid (Statem m i t a) where
  mempty = empty mzero
  mappend = (<+>)

(==>) :: State i -> (t, State i) -> Statem m i t a -> Statem m i t a
(==>) s (tr, s') statem  = connect s tr s' statem
