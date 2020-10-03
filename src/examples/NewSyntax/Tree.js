"use strict"

exports.readJSONJS = filecontents => JSON.parse(filecontents)

// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.d3HierarchyLinks = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.d3HierarchyDescendants = tree => tree.descendants()

// foreign import d3Hierarchy :: forall a. Tree a -> D3Hierarchical
exports.d3Hierarchy = json => d3.hierarchy(json).sort((a, b) => d3.ascending(a.data.name, b.data.name))

// foreign import d3Hierarchy :: forall a. D3Hierarchical -> D3Tree
exports.d3InitTree = config => hierarchy => d3.tree().size(config.size).separation(config.separation)(hierarchy)

// foreign import hasChildren :: Datum -> Boolean
exports.hasChildren = function(d) {
  return !d.children
}

// this REQUIRES that the data have been passed thru the d3.herarchy (and maybe d3.tree?) function
// NB because this is called directly from deep in the bowels of D3 (tree first walk to be precise) it isn't a curried function
exports.radialSeparationJS = (a,b) => (a.parent == b.parent ? 1 : 2) / a.depth