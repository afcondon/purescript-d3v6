module NewSyntax.Matrix where

import D3.Base

import D3.Base.Element (table_)
import D3.Base.Selection (withoutAttributes)
import Data.Tuple (Tuple(..))
import Prelude (identity, negate, ($), (*), (/))
import Unsafe.Coerce (unsafeCoerce)

type D3DOMelement = Int

type Model a = Array (Array a)

matrix :: Model Int
matrix = [
  [11975,  5871, 8916, 2868],
  [ 1951, 10048, 2060, 6171],
  [ 8010, 16145, 8090, 8045],
  [ 1013,   990,  940, 6907]
]
{-
d3.select("body")
    .append("table")
    .selectAll("tr")
  .data(matrix)
      .join("tr")
      .selectAll("td")
    .data(d => d)
      .join("td")
        .text(d => d);
-}
chartInit :: Selection (Model Int)
chartInit (Tuple width height) = do
  let origin = { x: -width / 2.0, y: -height / 2.0 }

  selectInDOM "div#matrix" [] [                         -- d3.select("body")
    table_                                              -- .append("table")
      [ joinElement_ "rows" Tr $                               -- .selectAll("tr").data(matrix).join("tr")
          joinElement_ "elements" Td $ enter withoutAttributes -- .selectAll("td").data(d => d).join("td")
            [ text (\d -> d) ]                          -- .text(d => d)
      ]    
  ] 
