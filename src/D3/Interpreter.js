use strict

// :: NativeSelection -> String -> NativeJS -> Unit
exports.runSimpleAttrJS = selection => attr => value => {
  console.log(`runSimpleAttrJS ${attr} ${value}`);
} 

// :: forall f. NativeSelection -> String -> f -> Unit
exports.runDatumAttrJS = selection => attr => f => {
  console.log(`runDatumAttrJS ${attr} f`)
}

// :: forall f. NativeSelection -> String -> f -> Unit
exports.runDatumIndexAttrJS = selection => attr => f =>
  console.log(`runDatumAttrJS ${attr} f`)


// :: String -> NativeSelection
exports.d3SelectAllJS = selector => d3.selectAll(selector) 

// :: NativeSelection -> String -> NativeSelection
exports.d3AppendElementJS = selection => element => selection.append(element) 

// :: forall d. d -> NativeSelection
exports.d3JoinJS = selection => data => selection.data(data)

// :: NativeSelection
exports.nullSelectionJS = null // NB only used on init, maybe still a bad idea
