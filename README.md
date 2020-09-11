# purescript-d3v6
Taking yet another run at this problem - armed with IxMonad this time, if necessary.

(see other repos in my GitHub for notes and previous attempts)

This go round the plan is
  * not to attempt to mirror the D3 JS APIs in any close way
  * to use indexed monads to enable chaining of Selection and Transition while still having some things that are particular to each and the ability to go from one to the other (and back) in the course of a single `do` block
  * maximize the utility of things that PureScript does perfectly well natively (such as reading and parsing data files, containers such as Maps, Sets, Graphs etc) 
  * keep in mind the possibility of a WebSocket-REST API in addition to the FFI so that for example programs could be run in Node but render in browsers


# status as of this commit

Abstract syntax tree of chart (for simple force layout only) now exists and also an interpreter (generic, not specific to the force layout) to do the effectful business of putting the chart into the DOM. The mainline is reading and decoding the exact same JSON file as in the standard D3 example and interpreting this chart using that model data. 

the FFI implmentations underneath the interpreter are stubbed out to console. 

# example of current syntax model

```purescript
chart :: Tuple Number Number -> Selection Model
chart (Tuple width height) = 
  initialSelect "div#force" "forceLayout" [] $ [
    appendNamed "svg" Svg [ viewBox 0.0 0.0 width height ] [
      append Group [ strokeColor "#999", strokeOpacity 0.6 ] 
        [ join Line modelLinks
          (appendNamed "link" Line [ strokeWidth_D (\d -> sqrt (d3Link d).value)] [])
          noUpdate noExit ]
        
      , append Group [ strokeColor "#fff", strokeOpacity 1.5 ]
        [ join Circle modelNodes
          (appendNamed "node" Circle [ radius 5.0, fill_D colorByGroup] [])
          noUpdate noExit ]
      ]
  ]
```

## Compare to this JS for example

```JavaScript
  const svg = d3.selectAll(element).append("svg")
      .attr("viewBox", [0, 0, width, height]);

  const link = svg.append("g")
      .attr("stroke", "#999")
      .attr("stroke-opacity", 0.6)
    .selectAll("line")
    .data(links)
    .join("line")
      .attr("stroke-width", d => Math.sqrt(d.value));

  const node = svg.append("g")
      .attr("stroke", "#fff")
      .attr("stroke-width", 1.5)
    .selectAll("circle")
    .data(nodes)
    .join("circle")
      .attr("r", 5) // altho static, can't promote
      .attr("fill", d => scale(d.group))
```

# Work in Progress / Design ideas and decisions

## Replicate the Data munging parts
The exception to this might be things that are impossible to represent in PureScript in a way that can be passed across the FFI. But against this, in turn, is the possibility of passing only JSON across the FFI so that there will always be a transformation that can handle this???

A concrete example: the D3 hierarchy structure by default expects the children field to be undefined in child nodes. If you want to represent a tree on the PureScript side as for example `data Tree a = N a (List Tree a)` you'd probably find it most natural to generate records of the form  `{ value :: a, children :: List a }` to pass to D3 but you'd need to null out those `children` fields...

Tentative decision - replicate the munging, but pick a statically typed standard for things like hierarchy and either convert before sending or send JSON that can be parsed correctly by D3's own defaults

CURRENT SOLUTION - using judicious `unsafeCoerce`-based functions to just work with the JavaScript-native data. Seems like a good compromise to retain readability of abstract chart definition (primary goal). Importantly, the writer of the "D3" script DSL doesn't do the unsafe coercions, they're handled by the internals.

## Use something more like Variant for attributes
Simple values probably not a problem but all the `.attr("foo", d => d.bar * 2)` or, worse, `.attr("foo", d => purescriptFn(d))` cases might be more complicated

Tentative decision - try to take the stuff from previous attempt first

## Indexed Monad types
  * Open Selection (such as `d3.selectAll('div#hook').append('circle')`), ie starting with d3 itself and going up to the point where you do the data/join
  * Operating Selection (from data.join) -> can produce a Transition
  * Transition -> can yield back to the selection
  
CURRENT STATUS - with the StateT tracking the context there doesn't seem to be any need for IndexedMonads actually. KISS principle.
  
