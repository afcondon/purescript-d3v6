# purescript-d3v6
Taking yet another run at this problem - armed with IxMonad this time

(see other repos in my GitHub for notes and previous attempts)

This go round the plan is
  * not to attempt to mirror the D3 JS APIs in any close way
  * to use indexed monads to enable chaining of Selection and Transition while still having some things that are particular to each and the ability to go from one to the other (and back) in the course of a single `do` block
  * maximize the utility of things that PureScript does perfectly well natively (such as reading and parsing data files, containers such as Maps, Sets, Graphs etc) 
  * keep in mind the possibility of a WebSocket-REST API in addition to the FFI so that for example programs could be run in Node but render in browsers


# status as of this commit

This is the baseline for experimentation, there is no DSL at all, just two D3 scripts living in PureScript FFI files and called directly from Main.

The goal from here is to replicate the functionality of this baseline using DSL so that all the FFI code is abstracted into some kind of wrapper library. 


# Work in Progress / Design ideas and decisions

## Replicate the Data munging parts
The exception to this might be things that are impossible to represent in PureScript in a way that can be passed across the FFI. But against this, in turn, is the possibility of passing only JSON across the FFI so that there will always be a transformation that can handle this???

A concrete example: the D3 hierarchy structure by default expects the children field to be undefined in child nodes. If you want to represent a tree on the PureScript side as for example `data Tree a = N a (List Tree a)` you'd probably find it most natural to generate records of the form  `{ value :: a, children :: List a }` to pass to D3 but you'd need to null out those `children` fields...

Tentative decision - replicate the munging, but pick a statically typed standard for things like hierarchy and either convert before sending or send JSON that can be parsed correctly by D3's own defaults

## Use something more like Variant for attributes
Simple values probably not a problem but all the `.attr("foo", d => d.bar * 2)` or, worse, `.attr("foo", d => purescriptFn(d))` cases might be more complicated

Tentative decision - try to take the stuff from previous attempt first

## Indexed Monad types
  * Open Selection (such as `d3.selectAll('div#hook').append('circle')`), ie starting with d3 itself and going up to the point where you do the data/join
  * Operating Selection (from data.join) -> can produce a Transition
  * Transition -> can yield back to the selection
  