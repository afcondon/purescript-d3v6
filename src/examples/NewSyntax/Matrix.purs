module NewSyntax.Matrix where

import D3.Base 

import Data.Tuple (Tuple(..))
import Prelude hiding (join)
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
chartInit :: Tuple Number Number -> Selection (Model Int)
chartInit (Tuple width height) = do
  let origin = { x: -width / 2.0, y: -height / 2.0 }

  selectInDOM "div#matrix" [] [ 
    table []
      [ join $ 
          Enter Td $ Child $
            join $
              Enter Tr $ Attrs [ computeText (\d -> unsafeCoerce d) ]
      ]
  ] 
