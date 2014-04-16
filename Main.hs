import Tracked
import Data.List
import Data.Maybe
import Control.Monad

--matchString :: String -> m Int Char
matchString s =
  let states = map state [1..]
  in chainFrom (state 0) (zip s states)

aab_star :: DFA Int Char Char
aab_star = fromJust $ dfa_connect (state 2) 'b' (state 0) $ matchString "aa"

--aab_star = aab_star'

aab_or_ab_star :: NFA Int Char Char
aab_or_ab_star = nfa_connect (state 1) 'a' (state 0) $ nfa_connect (state 2) 'b' (state 0) $ matchString "aa"

main = do
  putStrLn $ "DFA: " ++ show aab_star
  print $ applyAll "aabaab" (aab_star) --FAILS
--  putStrLn "NFA (aa):"  
--  print $ applyAll "aa" aab_or_ab_star
--  putStrLn "NFA (aab):"
--  print $ applyAll "aab" aab_or_ab_star
--  putStrLn "NFA (aaaaaab):"
--  print $ applyAll "aaaaaab" aab_or_ab_star
    

    --  mapM_ print (transitions $ fromJust test)
--  print $ fromJust (test >>= current)
