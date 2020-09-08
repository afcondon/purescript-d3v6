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

exports.initSimulationJS = config => {
  return d3.forceSimulation()
            .alpha(config.alpha) // default is 1
            .alphaTarget(config.alphaTarget) // default is 0
            .alphaMin(config.alphaMin) // default is 0.0001
            .alphaDecay(config.alphaDecay) // default is 0.0228
            .velocityDecay(config.velocityDecay) // default is 0.4
}
//  :: Simulation -> Array Node -> Unit
exports.putNodesInSimulationJS = simulation => nodes => simulation.nodes(nodes)
//  :: Simulation -> Array Link -> Unit
exports.putLinksInSimulationJS = simulation => links => simulation.links(links)
//  :: Unit -> Unit
exports.startSimulationJS = simulation => simulation.restart()
//  :: Unit -> Unit
exports.stopSimulationJS = simulation => simulation.stop()

// :: Simulation -> Unit
exports.forceManyJS = simulation => label => simulation.force(label, d3.forceManyBody())
// :: Simulation -> Number -> Number -> Unit
exports.forceCenterJS = simulation => label => cx => cy => simulation.force(label, d3.forceCenter(cx,cy))
// :: Simulation -> Unit
// exports.forceLinks = simulation => label => simulation.x()
// :: Simulation -> Number -> Unit
exports.forceCollideJS = simulation => label => radius => simulation.force(label, d3.forceCollide(radius))
// :: Simulation -> Number -> Unit
exports.forceXJS = simulation => label => cx => simulation.force(label, d3.forceX(cx))
// :: Simulation -> Number -> Unit
exports.forceYJS = simulation => label => cy => simulation.force(label, d3.forceY(cy))
// :: Simulation -> Number -> Number -> Unit
exports.forceRadialJS = simulation => label => cx => cy => simulation.force(label, d3.forceRadial(cx, cy))
