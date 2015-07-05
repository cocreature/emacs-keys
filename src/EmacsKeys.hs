{-# LANGUAGE TemplateHaskell #-}
-- | Main module that reexports the other ones
module EmacsKeys
  (module EmacsKeys.Parser
  ,module EmacsKeys.TH) where

import EmacsKeys.Parser
import EmacsKeys.TH
