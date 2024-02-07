module TestFeedback where 
import ApplyAll
import Utils
import ExpStrat
import ExpRules
import qualified Data.Map as Map


--test :: Rational -> IO()
test = do 
   r' <- getLine
   let r = floatToR r'
   let d = diagnose exponS (start [37, 41, 58, 68, 55]) (Map.singleton "inter" r)
   return d


