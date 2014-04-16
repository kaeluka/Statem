import Statem
import Data.List
import Data.Maybe
import Control.Monad

matchString :: String -> DFA Int Char
matchString s =
  let edges = map (==) s
      states = map state [1..]
  in chainFrom (state 0) (zip edges states)

test = connect (state 2) (=='b') (state 0) $ matchString "aa"


--ab_star =
--  (connect (mkState "a") (edge_eq "a") (mkState "b")) `loop` (edge_eq "b")

--runregex :: Statem Char -> String -> Statem Char
--runregex st s = foldl' apply st s

main = do
--  mapM_ print (transitions $ fromJust test)
--  print $ fromJust (test >>= current)
  putStrLn "arst"
