"use strict"

exports.readJSONJS = filecontents => decodeFile(filecontents)

const decodeFile = function (filecontents) {
  const json = JSON.parse(filecontents)
  const links = json.links.map(d => Object.create(d))
  const nodes = json.nodes.map(d => Object.create(d))
  return { links: links, nodes: nodes }
}

// foreign import d3HierarchyLinks :: D3Tree -> SubModel
exports.d3HierarchyLinks = tree => tree.links()

// foreign import d3HierarchyDescendents :: D3Tree -> SubModel
exports.d3HierarchyDescendents = tree => tree.descendents()

// foreign import d3Hierarchy :: forall a. Tree a -> D3Tree
exports.d3Hierarchy = tree => d3.d3Hierarchy(tree)

// foreign import hasChildren :: Datum -> Boolean
exports.hasChildren = d => typeof d.children != 'undefined'

exports.radialSeparationJS = a => b => (a.parent == b.parent ? 1 : 2) / a.depth