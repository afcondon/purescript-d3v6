"use strict"

//            COLOR & SCALE functions
// gross simplification here, scales can take ranges and allsorts
// we just want to be able to pass d3.schemeCategory10 back in from Purescript to prove the idea tho
const d3SchemeCategory10 = d3.scaleOrdinal(d3.schemeCategory10)
exports.d3SchemeCategory10JS = value => d3SchemeCategory10(value)
