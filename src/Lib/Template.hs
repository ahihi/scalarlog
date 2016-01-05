{-# LANGUAGE TemplateHaskell #-}
module Lib.Template
    ( cassiusTemplate
    , hamletTemplate
    , juliusTemplate
    ) where

import Text.Cassius
import Text.Hamlet
import Text.Julius

cassiusTemplate name = cassiusFile $ "src/" ++ name ++ ".cassius"
hamletTemplate name = hamletFile $ "src/" ++ name ++ ".hamlet"
juliusTemplate name = juliusFile $ "src/" ++ name ++ ".julius"