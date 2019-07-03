module Servant.API.NoContent
  ( NoContent(..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson)

data NoContent = NoContent

instance decodeNoContent :: DecodeJson NoContent where
  decodeJson = const $ pure NoContent

