module NewSyntax.GUP where

import D3.Base
import Data.Tuple (Tuple(..))
import Prelude (negate, ($), (*), (/))
import Unsafe.Coerce (unsafeCoerce)

-- with such simple data and simple data viz, the data in the DOM will be exactly same as Model data
-- this means that a simple coerce suffices to turn Datum -> D3DOMelement and also that no Projection
-- function is required, hence the use of joinElement_ 
type D3DOMelement = Char

type Model a = Array a

chartInit :: Tuple Number Number -> Selection (Model Char)
chartInit (Tuple width height) = do
  let origin = { x: -width / 2.0, y: -height / 2.0 }
      t = [ Duration 750 ]
      xPosition d i = i * 16.0

  selectInDOM "div#GUP" [] [-- this is an InitialSelect
    svg 
      [ viewBox origin.x origin.y width height ]
      [ join $
          EnterUpdateExit Text 
            -- Enter
            (Attrs  [ fill "green"
                    , y (-30.0) Px
                    , computeXusingIndex Px xPosition
                    , computeText (\d -> unsafeCoerce d )
                    -- , transition t -- TODO maybe this can become proper selection child now?
                    , y 0.0 Px ]) 
            -- Update
            (Attrs  [ fill "black",  y 0.0 Px
                    -- , transition t -- TODO make this a proper child later
                    , computeXusingIndex Px xPosition ])
            -- exit 
            (Attrs  [ fill "brown", y 30.0 Px
                    -- , transition t -- TODO make this a proper child later
                    , y 30.0 Px ])
      ]    
  ] 


-- const t = svg.transition()
--     .duration(750);

-- svg.selectAll("text")
--   .data(randomLetters(), d => d)
--   .join(
--     enter => enter.append("text")
--         .attr("fill", "green")
--         .attr("x", (d, i) => i * 16)
--         .attr("y", -30)
--         .text(d => d)
--       .call(enter => enter.transition(t)
--         .attr("y", 0)),
--     update => update
--         .attr("fill", "black")
--         .attr("y", 0)
--       .call(update => update.transition(t)
--         .attr("x", (d, i) => i * 16)),
--     exit => exit
--         .attr("fill", "brown")
--       .call(exit => exit.transition(t)
--         .attr("y", 30)
--         .remove())
--   );

