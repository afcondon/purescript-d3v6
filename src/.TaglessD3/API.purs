module TaglessD3.API where

import D3.Base (Hierarchy)
import D3.Transition (D3Transition, TimeSpec)
import Data.List (List)
import Prelude (class Monad, Unit)
import TaglessD3.AttrNew (Attr)
import TaglessD3.Base (D3ElementType, Selector)

data D3Data d i = ArrayD     (Array d)
                | HierarchyD (Hierarchy d)
                | ArrayDI     (Array d)     (d -> i)
                | HierarchyDI (Hierarchy d) (d -> i)

class (Monad m) <= AbstractD3API m where
    append      :: D3ElementType -> (m Unit)
    attrs       :: List Attr     -> (m Unit)
    d3Select    :: Selector      -> (m Unit)
    d3SelectAll :: Selector      -> (m Unit)
    dataBind    :: ∀ d i. D3Data d i -> (m Unit)
    enter       ::                  (m Unit)
    exit        ::                  (m Unit)
    insert      :: D3ElementType -> (m Unit)
    merge       :: m Unit        -> (m Unit)
    remove      ::                  (m Unit)
    select      :: Selector      -> (m Unit)
    selectAll   :: Selector      -> (m Unit)
    applyTransition :: m Unit    -> (m Unit) -- effectively this is like a merge, for now. not optimal.
-- the following methods on D3's selection are not yet implemented in tagless form
    -- call
    -- classed -- implemented as part of attr(s)
    -- datum
    -- dispatch
    -- each
    -- empty
    -- filter
    -- html
    -- interrupt
    -- lower
    -- node(s)
    -- on
    -- order
    -- property(ies) -- implemented as part of attr(s)
    -- raise
    -- size
    -- sort
    -- style(s) -- implemented as part of attr(s)
    -- text
-- the methods that apply to D3 Transitions
    tMerge       :: m Unit        -> (m Unit)
    tRemove      ::                  (m Unit)
    tSelect      :: Selector      -> (m Unit)
    tSelectAll   :: Selector      -> (m Unit)
    makeTransition  :: D3Transition  -> (m Unit)
    delay       :: ∀ d. TimeSpec d -> (m Unit)
    duration    :: ∀ d. TimeSpec d -> (m Unit)
    -- the following methods on D3's transition are not currently implemented in the tagless version
        -- attrTween
        -- call
        -- d3Interrupt
        -- delay
        -- duration
        -- each
        -- ease
        -- empty
        -- filter
        -- node(s)
        -- on
        -- selection
        -- size
        -- style(s) -- part of attr(s)
        -- styleTween
        -- text
        -- transition
        -- tween
