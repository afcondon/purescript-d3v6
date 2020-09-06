use strict

exports.readJSONJS = jsondata => decodeFile(filecontents)

const jsondata = function (filecontents) {
  const json = JSON.parse(filecontents)
  const links = json.links.map(d => Object.create(d))
  const nodes = json.nodes.map(d => Object.create(d))
  return { links: links, nodes: nodes }
}