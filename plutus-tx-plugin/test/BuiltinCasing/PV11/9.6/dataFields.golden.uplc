(program
   1.1.0
   ((\forced forced d ->
       (\dataConstr ->
          case (forced dataConstr) [(\xs -> xs)] (forced dataConstr))
         (unConstrData d))
      (force (force fstPair))
      (force (force sndPair))))