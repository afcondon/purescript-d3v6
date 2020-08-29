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