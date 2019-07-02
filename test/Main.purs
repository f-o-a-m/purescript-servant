module Test.Main where

import Prelude

import Control.Monad (join)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Servant.Spec.ClientSpec (mkClientEnv)
import Servant.Spec.ClientSpec as ClientSpec
import Servant.Spec.PhotoServer (startApp)
import Test.Spec (beforeAll_, describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

main :: Effect Unit
main = launchAff_ $ join $ runSpecT defaultConfig [consoleReporter] do
  describe "ClientSpec" do
    let port = 8080
    beforeAll_ (void $ startApp port) $ do
      let clientEnv = mkClientEnv port
      ClientSpec.spec clientEnv
