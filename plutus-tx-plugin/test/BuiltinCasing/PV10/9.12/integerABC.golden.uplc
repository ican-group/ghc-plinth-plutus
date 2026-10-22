(program
   1.1.0
   ((\forced i ->
       force
         (case
            (constr 0
               [ (equalsInteger 0 i)
               , (delay "a")
               , (delay
                    (force
                       (case
                          (constr 0
                             [ (equalsInteger 1 i)
                             , (delay "b")
                             , (delay
                                  (force
                                     (case
                                        (constr 0
                                           [ (equalsInteger 2 i)
                                           , (delay "c")
                                           , (delay error) ])
                                        [forced]))) ])
                          [forced]))) ])
            [forced]))
      (force ifThenElse)))