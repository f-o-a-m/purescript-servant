{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "servant"
, dependencies =
  [ "aff"
  , "argonaut"
  , "prelude"
  , "affjax"
  , "errors"
  , "heterogeneous"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
