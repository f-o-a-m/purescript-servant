{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "servant-tests"
, dependencies =
  [ "prelude"
  , "argonaut"
  , "servant"
  , "console"
  , "spec"
  , "foreign-generic"
  , "express"
  ]
, packages = ./packages.dhall
, sources = [ "test/**/*.purs" ]
}
