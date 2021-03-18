module D3.Base.Element  where

import D3.Base.Attributes (Attr)
import D3.Base.Selection (Element(..), Selection, append, append_)

-- Group and Div are containers so the underscore versions elide attrs, not children
-- for a deeper consistency
group :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
group = append Group 

group_ :: forall model.              Array (Selection model)
  -> Selection model 
group_ = append Group []

div :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
div = append Div 

div_ :: forall model.              Array (Selection model)
  -> Selection model 
div_ = append Div []



svg :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
svg = append Svg 

svg_ :: forall model. Array Attr 
  -> Selection model 
svg_ = append_ Svg

line :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
line = append Line 

line_ :: forall model. Array Attr 
  -> Selection model 
line_ = append_ Line

circle :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
circle = append Circle 

circle_ :: forall model. Array Attr 
  -> Selection model 
circle_ = append_ Circle

path :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
path = append Path 

path_ :: forall model. Array Attr 
  -> Selection model 
path_ = append_ Path

text :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
text = append Text 

text_ :: forall model. Array Attr 
  -> Selection model 
text_ = append_ Text



table :: forall model. Array Attr -> Array (Selection model)
  -> Selection model 
table = append Table 

table_ :: forall model. Array Attr 
  -> Selection model 
table_ = append_ Table
