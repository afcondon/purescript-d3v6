

simulation =
  Simulation "simulation"
    [ Tuple "charge" ForceMany
    , Tuple "center" (forceCenter width / 2 height / 2) ] 
    [ Tick (tickFn "links" "nodes")]

chart = 
  Visualization "div#chart" [
      Append "svg" Svg [
        Append "link" G Line somedata [
          Join enter update exit
        ]
    , Append "node" G Circle somedata [
        Join 
          Enter enter
          Update noop
          Exit noop
      ]
    ] 
  ]

drag = 
  d3Drag "node" simulation {
      start: dragstarted
    , drag:  dragged
    , end:   dragended
  }


