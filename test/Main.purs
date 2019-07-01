module Test.Main where

import Prelude

import Effect (Effect)
import Servant.Spec.ClientSpec as ClientSpec

main :: Effect Unit
main = do
  ClientSpec.spec
