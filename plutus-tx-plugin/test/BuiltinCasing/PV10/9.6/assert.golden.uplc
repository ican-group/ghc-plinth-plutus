(program
   1.1.0
   ((\forced ds ->
       force (case (constr 0 [ds, (delay ()), (delay error)]) [forced]))
      (force ifThenElse)))