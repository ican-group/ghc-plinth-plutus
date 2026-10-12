(program
   1.1.0
   (case
      (constr 0
         [(force tailList), (force headList), (force (force chooseList))])
      [ (\forced forced forced xs ->
           force
             (case
                (constr 0
                   [ xs
                   , (delay (\ds -> error))
                   , (delay ((\x -> (\xs ds -> x) (forced xs)) (forced xs))) ])
                [forced])
             ()) ]))