### Changed

- Under plinthc, the type checker now checks every Plinth splice (the Plinth
  gate, error code `PLINTH-00008`) before the plugin runs. A failure of the
  Core to PIR compile after the gate accepted the code is reported as an
  internal error with the code `PLINTH-00009`. The option `defer-errors`
  also turns the gate off, and the module-wide rejection of the `GADTs`
  and `PolyKinds` extensions is left to the gate, which checks each
  declaration.
