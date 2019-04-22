~coalStep :: R -> Model -> IO Model~
coalStep t m = coal t `rjmc` coalMove t `runStep` m