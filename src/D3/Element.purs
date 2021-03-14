module D3.Element  where

import D3.Attributes (Attr)
import D3.Selection (Element(..), Selection, append, append_)

svg :: forall model. Array Attr -> Array (Selection model) -> Selection model 
svg = append Svg 

svg_ :: forall model. Array Attr                            -> Selection model 
svg_ = append_ Svg

group :: forall model. Array Attr -> Array (Selection model) -> Selection model 
group = append Group 

group_ :: forall model. Array Attr                            -> Selection model 
group_ = append_ Group

div :: forall model. Array Attr -> Array (Selection model) -> Selection model 
div = append Div 

div_ :: forall model. Array Attr                            -> Selection model 
div_ = append_ Div

line :: forall model. Array Attr -> Array (Selection model) -> Selection model 
line = append Line 

line_ :: forall model. Array Attr                            -> Selection model 
line_ = append_ Line

circle :: forall model. Array Attr -> Array (Selection model) -> Selection model 
circle = append Circle 

circle_ :: forall model. Array Attr                            -> Selection model 
circle_ = append_ Circle

path :: forall model. Array Attr -> Array (Selection model) -> Selection model 
path = append Path 

path_ :: forall model. Array Attr                            -> Selection model 
path_ = append_ Path

text :: forall model. Array Attr -> Array (Selection model) -> Selection model 
text = append Text 

text_ :: forall model. Array Attr                            -> Selection model 
text_ = append_ Text

