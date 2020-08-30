

simulation =
  Simulation "simulation"
    [ Tuple "charge" ForceMany
    , Tuple "center" (forceCenter width / 2 height / 2) ] 
    [ Tick (tickFn "links" "nodes")]

chart = Visualization "div#chart" [
            Append "svg" Svg [
              Append "links" G Line somedata [
                Join enter update exit
              ]
          , Append "nodes" G Circle somedata [
              Join 
                Enter enter
                Update noop
                Exit noop
            ]
        ]
      ]
    ]

