"use strict"

//            ATTR functions

// :: NativeSelection -> String -> NativeJS -> Unit
exports.runSimpleAttrJS     = selection => attr => value => selection.attr(attr, value)
// :: forall f. NativeSelection -> String -> f -> Unit
exports.runDatumAttrJS      = selection => attr => f => selection.attr(attr, f)

// NB the (d,i)             => f(d)(i) in runDatumIndexAttrJS - the attribute-setter
// function is not curried, since it’s been written on the PureScript side

// :: forall f. NativeSelection -> String -> f -> Unit
exports.runDatumIndexAttrJS = selection => attr => f => selection.attr(attr, (d,i) => f(d)(i))
// foreign import runDatumTextJS      :: forall f. NativeSelection           -> f -> Unit
exports.runDatumTextJS      = selection => f => selection.text(f)

//           SELECTION functions
// :: String -> NativeSelection
exports.d3SelectAllJS       = selector => d3.selectAll(selector)
// :: NativeSelection -> String -> NativeSelection
exports.d3AppendElementJS   = selection => element => selection.append(element)
// :: forall d. d -> NativeSelection
exports.d3JoinJS = selection => element => data => fns => { 
  console.log("d3JoinJS");
  return selection.selectAll(element).data(data).join(fns.enter, fns.update, fns.exit)
}

exports.d3EnterWithIndexJS  = selection => data => idFunction => selection.data(data, idFunction)
// :: NativeSelection
exports.nullSelectionJS     = null // NB only used on init, maybe still a bad idea


//            SIMULATION functions
exports.initSimulationJS = config => {
  return d3.forceSimulation()
            .alpha(config.alpha) // default is 1
            .alphaTarget(config.alphaTarget) // default is 0
            .alphaMin(config.alphaMin) // default is 0.0001
            .alphaDecay(config.alphaDecay) // default is 0.0228
            .velocityDecay(config.velocityDecay) // default is 0.4
}
//  :: Simulation -> Array NativeNode -> Array NativeNode
exports.putNodesInSimulationJS = simulation => nodes => { 
  simulation.nodes(nodes)
  return nodes
}
//  :: Simulation -> Array NativeLink -> Array NativeLink
exports.putLinksInSimulationJS = simulation => links => { 
  simulation.force("links", d3.forceLink(links).id(d => d.id))
  return links
}
// :: NativeSelection -> Number -> Unit
exports.setAlphaTargetJS = simulation => target => simulation.alphaTarget(target)
//  :: NativeSelection -> Unit
exports.startSimulationJS = simulation => simulation.restart()
//  :: NativeSelection -> Unit
exports.stopSimulationJS = simulation => simulation.stop()

var tickAttrArray = [] // TODO probably want API to reset this too, but defer til adding named tick functions
exports.addAttrFnToTickJS = selection => pursAttr => {
  tickAttrArray.push({ selection: selection, attr: pursAttr.value0, fn: pursAttr.value1 })
} 
// assumes we've already put all the things we want to happen into the tickAttrArray
exports.attachTickFnToSimulationJS = simulation => simulation.on("tick", () => {
  tickAttrArray.forEach(element => (element.selection).attr(element.attr, element.fn))
})
// default drag function, useful in probably most simulations
exports.attachDefaultDragBehaviorJS = selection => simulation => {
  
  var drag = function(simulation) {
    function dragstarted(event, d) {
      if (!event.active) simulation.alphaTarget(0.3).restart();
      d.fx = d.x;
      d.fy = d.y;
    }
    
    function dragged(event,d) {
      d.fx = event.x;
      d.fy = event.y;
    }
    
    function dragended(event,d) {
      if (!event.active) simulation.alphaTarget(0);
      d.fx = null;
      d.fy = null;
    }
    
    return d3.drag()
        .on("start", dragstarted)
        .on("drag", dragged)
        .on("end", dragended);  
  }

  selection.call(drag(simulation))
}

//            FORCE functions 
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

