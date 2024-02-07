module Main where
import ExpStrat
import Diagnose
import FSM
import Data.Maybe
import Parameters
import qualified ApplyAll as A

main :: IO()
main = print $ params2 (space 2) 
   --mapM_ print $ fromMaybe [] $ A.diagnose (exponS) test1 test2
