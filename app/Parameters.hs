module Parameters where
import Refinement
import qualified ExpRules as R (start)
import FSM 
import Utils
import ExpStrat
import BWSGBuilder 
import StrategyBas
import FSM
import Prune
import ApplyAll
import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

getStates :: Ord a => [Int] -> RGraph a -> Set a 
getStates ns rg = 
   Set.unions $ map Map.keysSet $ listLookup ns rg  

sizeFinal :: Ord a => FSMR a -> a -> Int 
sizeFinal fsm a = Set.size $ getStates (accept fsm) (build fsm a)

sizePoint :: Ref Rational -> Int
sizePoint p = sizeFinal (pruneBFSM 5 $ fsm exponS) $ C (p, 0)

sizePointA :: Ref Rational -> Int
sizePointA p = Set.size $ Set.fromList $ applyAll exponS p 

space' :: [[Rational]]
space' = [[rn (x1/2), rn (x2/2), rn (x3/2), rn (y1/2), g/100] | 
         [x1, x2, x3, y1, g] <- rmL 5 50 199 
         , (x2-x1)/2 > 3
         , (x3-x2)/2 > 3
         , y1 * (g/100)^round ((x2-x1)/2) < 200
         , abs (y1 * (g/100)^round ((x2-x1)/2) - y1) > 2 
         , y1 * (g/100)^round ((x3-x1)/2) < 1000
         , y1 * (g/100)^round ((x2-x1)/2) > 2
         , y1 * (g/100)^round ((x3-x1)/2) > 2
         ] 
   where rn = fromIntegral.round

space :: Int -> [Ref Rational]
space n =
   take n 
      [R.start [x1, x2, y1, rn $ y1 * g ^ (round (x2-x1)), x3] 
      | [x1,x2,x3,y1,g] <- space']
   where rn = fromIntegral.round

params2 sp = setNub $ zip (map (((-1)*) . sizePointA) sp) sp

params :: [Ref Rational] -> [Ref Rational]
params sp = sorter f sp
   where f = ((-1) *) . sizePointA