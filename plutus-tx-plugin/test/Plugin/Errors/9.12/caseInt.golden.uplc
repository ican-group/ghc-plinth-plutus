Error: Unsupported feature: Cannot pattern match on a value of type 'Integer'.

       This happens when the code, or a function from a GHC library that it
       uses, matches on the internal constructors of 'Integer' (IS, IP, IN).
       Plinth compiles 'Integer' to the builtin integer type, which has no
       constructors. Patterns on integer literals are supported:

         f 42 = ...
         f n  = ...

       Use the functions from 'PlutusTx.Prelude' on 'Integer', for example
       (==), compare, or divide.

       Note: GHC can generate these unexpectedly, you may need '-fno-strictness', '-fno-specialise', '-fno-spec-constr', '-fno-unbox-strict-fields', or '-fno-unbox-small-strict-fields'.