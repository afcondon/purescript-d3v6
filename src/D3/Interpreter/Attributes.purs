module D3.Interpreter.Attributes where

import D3.Interpreter.Foreign

import D3.Base (Attr(..), NativeSelection)
import Prelude (Unit, pure, ($))
import D3.Interpreter.Types (D3)

-- interprets and calls D3 directly, as opposed to storing attr associations on JS side,
-- as is needed for, for example tick function
applyAttr :: forall model. NativeSelection -> Attr -> D3 model Unit
applyAttr selection = case _ of
    (StaticString attr value)      -> pure $ runSimpleAttrJS selection attr (stringToNativeJS value)
    (StaticNumber attr value)      -> pure $ runSimpleAttrJS selection attr (numberToNativeJS value)
    (StaticArrayNumber attr value) -> pure $ runSimpleAttrJS selection attr (arrayNumberToNativeJS value)

    (StringAttr attr fnD)          -> pure $ runDatumAttrJS selection attr fnD
    (NumberAttr attr fnD)          -> pure $ runDatumAttrJS selection attr fnD
    (ArrayNumberAttr attr fnD)     -> pure $ runDatumAttrJS selection attr fnD

    (StringAttrI attr fnDI)        -> pure $ runDatumIndexAttrJS selection attr fnDI
    (NumberAttrI attr fnDI)        -> pure $ runDatumIndexAttrJS selection attr fnDI
    (ArrayNumberAttrI attr fnDI)   -> pure $ runDatumIndexAttrJS selection attr fnDI

    (TextAttr fnD)                 -> pure $ runDatumTextJS selection fnD
