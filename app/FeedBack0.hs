module FeedBack0 where 
import FeedBackI
import Data.Maybe
import Refinement

fb0 :: ParamFB 
fb0 rf =
   [ FB ([]==)

        "Dit is correct, goed zo!"
       
        (P 0)

        (S 0)
     
   
   , FB (["round final answer correctly 3"]==)

        "Dit is correct, goed zo!"
       
        (P 1)

        (S 0)

   , FB (["round final answer correctly"]==)

        ("Rond af op drie decimalen en zorg ervoor" ++
        "dat je tussentijds niet afrondt")
       
        (P 2)

        (S 0)

   , FB (anySub [["calculate: f = y2-y1 or f = y1-y2", "calculate: g = f/dx"]])
       
        ("Het lijkt wel alsof je de groeifactor als hellingsgetal hebt berekend, "++ 
          "maar het is nu toch exponentieel?")

        (P 3)

        (S 3)

   , FB ("calculate: f = y1/x1 or f = y2/x2" `elem`)
       
        ("Het lijkt wel alsof je bij de groeifactor de verhouding tussen x en y hebt gebruikt, "++ 
          "maar het is nu toch exponentieel?")

        (P 4)

        (S 3)  

   , FB ("calculate: f = y2-y1 or f = y1-y2" `elem`)
       
        ("Het lijkt wel alsof bij de groeifactor het verschil in y waarden hebt gebruikt, "++ 
          "maar het is nu toch exponentieel?")

        (P 5)

        (S 3)  

   , FB ("invert factor" `elem`)
       
        ("Is er sprake van toename of afname?")

        (P 6)

        (S 3)  

   , FB (anyElem ["calculate: dx = x1", "calculate: dx = x2"])
       
        ("Heb je de toename van x wel goed berekend?" ++ 
         " Hoeveel komt er bij x bij? ")

        (P 7)

        (S 3)

   , FB ("calculate: g = f/dx" `elem`)
       
        ("Het lijkt er op dat je de groeifactor per tijdseenheid met" ++ 
         " delen hebt berekend, klopt dat wel?")

        (P 8)

        (S 3)
   , FB ("calculate: g = f^dx" `elem`)
       
        ("Bij klopt de macht wel bij het berekenen van de groeifactor?" ++ 
         " Met de macht bereken je de groeifactor per stap van x")

        (P 8)

        (S 3)
   ]
   where 
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf)