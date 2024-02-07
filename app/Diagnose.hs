module Diagnose where
import Data.Maybe
import BWSGBuilder
import BackTrack
import Prune
import Rules
import NubSub
import FSM 
import ExpStrat
import StrategyBas
import qualified Data.Map as Map
import qualified Data.Set as Set

isBuggyE :: Maybe (Rule a) -> Bool
isBuggyE = fromMaybe False . fmap isBuggy

diagnose 
   :: Ord a => 
      FSMR a 
      -> a -> a 
      -> Maybe [L (Maybe (Rule a))]
diagnose fsm a1 a2 =
   Map.lookup a1' $ 
   backTrack isBuggyE g pts 
   where 
      a1' = (start fsm, a1)
      pts = [(s, a2) | s <- accept fsm]
      g = build fsm a1   

diagnoseMax
  :: Ord a =>
     Int
     -> FSMR a 
     -> a -> a 
     -> Maybe [L (Maybe (Rule (Ct a)))]
diagnoseMax n fsm a1 a2 =
   Map.lookup a1' $
   backTrack isBuggyE g pts 
   where 
      a1' = (start fsm, C (a1, 0))
      pts = [(s, C (a2, k)) | s <- accept fsm, k <- [0..n]]
      g = build (pruneBFSM n fsm) $ C (a1, 0) 