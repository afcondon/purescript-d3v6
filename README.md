# purescript-d3v6

## Approach taken / project goals

The goal has been to add some type-safety and composability to the construction of visualizations with D3.

The aim is to do this with ZERO compromise to D3js' graphical expressivity, ie want to be able to reproduce any D3js viz.

A secondary, but important goal is to improve readability.

*Non-goals* include replicating parts of D3js that would - IMHO - be better done in PureScript, data-processing and cleaning and so forth. 

Performance neither a goal or a non-goal, this library is only interpreting a kind of AST for the D3 chart, D3 is doing all the work under the hood including, importantly all the DOM updates and animations etc. The Purescript layer shouldn't be adding any overhead at all, practically speaking.

# status as of this commit

Abstract syntax tree of chart (for simple force layout only) now exists and also an interpreter (generic, not specific to the force layout) to do the effectful business of putting the chart into the DOM. The mainline is reading and decoding the exact same JSON file as in the standard D3 example and interpreting this chart using that model data. 


## example of current syntax model

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

# Quickstart to run an example

Note that this is for a different example than the above, and assumes you already have `purescript` and `spago` installed.
```
npm i
npm run example
```

