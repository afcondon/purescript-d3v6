"use strict"

exports.readJSONJS = filecontents => decodeFile(filecontents)

const decodeFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  return json
}

// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.d3HierarchyLinks = tree => tree.links()

// foreign import d3HierarchyDescendants :: D3Tree -> SubModel
exports.d3HierarchyDescendants = tree => tree.descendants()

// foreign import d3Hierarchy :: forall a. Tree a -> D3Hierarchical
exports.d3Hierarchy = tree => d3.hierarchy(tree)

// foreign import d3Hierarchy :: forall a. D3Hierarchical -> D3Tree
exports.d3InitTree = config => hierarchy => d3.tree().size(config.size).separation(config.separation)(hierarchy)

// foreign import hasChildren :: Datum -> Boolean
exports.hasChildren = d => typeof d.children != 'undefined'

exports.radialSeparationJS = a => b => (a.parent == b.parent ? 1 : 2) / a.depth