# purescript-d3v6
Taking yet another run at this problem - armed with IxMonad this time

(see other repos in my GitHub for notes and previous attempts.

This go round the plan is
  * not to wrap the D3 APIs in any close way (i'll write more later about this)
  * to use IxMonad to enable monadic chaining of Selection and Transition while still having some things that are particular to each and the ability to go from one to the other (and back) in the course of a single `do` block
  * maximize the utility of things that PureScript does perfectly well natively (such as reading and parsing data files, containers such as Maps, Sets, Graphs etc) 
  * keep in mind the possibility of a WebSocket-REST API in addition to the FFI so that for example programs could be run in Node but render in browsers
