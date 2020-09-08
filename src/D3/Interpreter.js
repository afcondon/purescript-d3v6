"use strict"

// :: NativeSelection -> String -> NativeJS -> Unit
exports.runSimpleAttrJS = selection => attr => value => {
  console.log(`runSimpleAttrJS ${attr} ${value}`);
  selection.attr(attr, value)
} 

// :: forall f. NativeSelection -> String -> f -> Unit
exports.runDatumAttrJS = selection => attr => f => {
  console.log(`runDatumAttrJS ${attr} f`)
  selection.attr(attr, f)
}

// :: forall f. NativeSelection -> String -> f -> Unit
exports.runDatumIndexAttrJS = selection => attr => f => {
  console.log(`runDatumIndexAttrJS ${attr} f`)
  selection.attr(attr, d => i => f(d)(i))
}

// :: String -> NativeSelection
exports.d3SelectAllJS = selector => { 
  return d3.selectAll(selector)
}

// :: NativeSelection -> String -> NativeSelection
exports.d3AppendElementJS = selection => element => { 
  console.log('append');
  return selection.append(element)
}

// // :: NativeSelection -> String -> NativeSelection
// exports.d3EnterElementJS = selection => element => { 
//   console.log('enter');
//   return selection.append(element)
// }

// :: forall d. d -> NativeSelection
exports.d3JoinJS = selection => element => data => { 
  console.log('joining the data to the model');
  // could we get the join.enter, join.update, join.exit selections out of this in order to simply attach the append normally later
  return selection.selectAll(element).data(data).enter()
}

exports.d3JoinWithIndexJS = selection => data => idFunction => { 
  return selection.data(data, idFunction)
}

// :: NativeSelection
exports.nullSelectionJS = null // NB only used on init, maybe still a bad idea
