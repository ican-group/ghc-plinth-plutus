(program
   1.1.0
   ((\forced forced p -> addInteger (forced p) (forced p))
      (force (force fstPair))
      (force (force sndPair))))