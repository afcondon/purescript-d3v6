
# analysis of d3 grammar

## top of mind ideas
  * maintain full expressivity of D3
  * no attempt to model imperative, chained model of D3
  * use StateT to hold the stateful elements like named selections, transitions, scales, interpolators etc
  * no performance / space optimizations required in Purs code, for example, entirely static attributes can be promoted from data-bound elements to parent as is done in D3 practice but this would be done by interpreter

## abstract grammar of force example
```
  make simulation w. [force] [config]
    add tick with reference to named selections

  make selection "div#chart" 
      append 'svg' [props] [
          append 'g' "links" [props] -- and export selection
            insert Line links
              ( Join  (Enter lines [props]) -- static props can be promoted to 'g'
                      (Update [])
                      (Exit []) )
        , append 'g' "nodes" [props] -- and export selection
            insert Circle nodes
              ( Join (Enter circles [props])
                     (Update [])
                     (Exit []) )
                call custom drag simulation -- causes simulation to wake up too
      ]
```

## abstract grammar of tree example
```
  make hierarchyData from data
  make treeFn [config]
  feed hierarchyData into treeFn
  make selection "div#app"
    append 'svg' props [
        append 'g' for links
          selectAll `path`
          data bind root.links
          join
            enter 'path' [props] -- one prop, 'd' is d3.linkRadial().angle(d=>d.x).radius(d=>d.y)
      , append 'g' for circles
          selectAll `circle`
          data bind root.descendants()
          join
            enter 'circle' props -- one prop, 'transform' is printf-ish thing
      , append 'g' for text
          selectAll `text`
          data bind root.descendants()
          join
            enter 'text' [props] -- one prop, 'transform' is printf-ish thing
          clone(true).lower() [props] -- clones whole text selection, lowers it, white outline
    ]
```

## abstract grammar of lsm.js

```

```

## abstract grammar of (new) GUP

```
  make selection "div#chart"
    append 'svg' w. [props] [ -- width, height, viewbox
        append 
```