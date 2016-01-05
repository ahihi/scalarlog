{-# LANGUAGE TemplateHaskell #-}
module Lib.Template
    ( hamletTemplate
    ) where

import Text.Hamlet

hamletTemplate name = hamletFile $ "src/" ++ name ++ ".hamlet"
